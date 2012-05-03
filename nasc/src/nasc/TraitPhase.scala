package nasc

class TraitPhase extends Phase[CompilationUnit, CompilationUnit] {

  def name = "trait"

  def execute(cu: CompilationUnit): CompilationUnit = {
    cu.root = cu.root.transform(insertTraitMembers)
    cu.root = cu.root.transform(rerouteCalls)
    cu.root = cu.root.transform(initVtable)
    cu.root = cu.root.transform(insertVtable)
    cu.root = cu.root.transform(virtualCalls)
    cu.root = cu.root.transformSymbols { (_, s) => virtualSymbolTranslation.getOrElse(s, s)}
    cu
  }

  def initVtable(st: Statement): Statement = st match {
    case sd: StructDefinition => {
      val symTrans = symbolTranslation(sd.typeSymbol)
      val vtableInit = sd.traits.flatMap { tr =>
        val td = tr.symbol.definition.asInstanceOf[TraitDefinition]
        val vtable = ValDefinition("__vtable_" + td.typeSymbol.uniqueName, TypeId.fromSymbol(td.typeSymbol), None, true)
        vtable.valSymbol = sd.vtableSymbols(tr.symbol)
        List(vtable)
        /*val vtableDef = vtable :: symTrans.foldLeft(List[Statement]()) {
          case (l, (k, v)) =>
            println("KV " + k + ", " + v)
            val vtableField = Select(Id.fromSymbol(vtable.valSymbol), k.name)
            vtableField.fieldSymbol = k
            vtableField.ty = k.ty
            val assign = Assign(vtableField, Id.fromSymbol(v))
            println("VTF " + vtable.valSymbol.ty.concreteType.memberSymbol("u"))
            assign :: l
        }
        vtableDef*/
      }
      val newStruct = sd.copy(body = Block(vtableInit ++ sd.body.content))
      sd.copyAttrs(newStruct.asInstanceOf[sd.type])
      Typer.typeStatement(newStruct)
      newStruct
    }
    case _ => st
  }

  var virtualSymbolTranslation: Map[Symbol, Symbol] = Map()

  def virtualCalls(s: Statement): Statement = s match {
    case c @ Call(sel @ Select(e, name), args) if virtualSymbolTranslation.contains(sel.fieldSymbol) => {
      sel.fieldSymbol = virtualSymbolTranslation(sel.fieldSymbol)
      sel.ty = sel.fieldSymbol.ty
      val call = Call(sel, e :: args)
      call.ty = c.ty
      call
    }
    case _ => s
  }

  def insertVtable(s: Statement): Statement = s match {
    case td: TraitDefinition => {
      val vtableBody = Block(
        td.body.children.flatMap {
          case fd: FunctionDefinition => {
            val funType = fd.functionType
            val entryType = Defs.types.Function.create(funType.retType, Types.Qual.addAttribute(Defs.types.Byte, Types.Attributes.Ref) :: funType.argTypes)
            val typeExpr = TypeId.fromSymbol(entryType.typeSymbol)
            val vd = ValDefinition(fd.name, typeExpr, None, true)
            vd.declareSymbols()
            vd.valSymbol.ty = entryType
            virtualSymbolTranslation += fd.funSymbol -> vd.valSymbol
            List(vd)
          }
          case _ => List()
        })
      val vtableDef = StructDefinition(td.name + "__vtable", List(), List(), vtableBody, true)
      vtableDef.declareSymbols()
      Typer.typeStatement(vtableDef)
      vtableDef
    }
    case _ => s
  }

  var symbolTranslation: Map[TypeSymbol, Map[Symbol, Symbol]] = Map()

  def rerouteCalls(s: Statement): Statement = s match {
    case sel @ Select(expr, _) => {
      symbolTranslation.get(expr.ty.typeSymbol) match {
        case Some(symTrans) => {
          sel.fieldSymbol = symTrans.getOrElse(sel.fieldSymbol, sel.fieldSymbol)
        }
        case None => ()
      }
      sel
    }
    case _ => s
  }

  def insertTraitMembers(s: Statement): Statement = s match {
    case sd: StructDefinition => {
      var symTrans = Map[Symbol, Symbol]()
      val processedStruct = sd.traits.foldLeft(sd) {
        case (sd, t) =>
          t.symbol.definition match {
            case td: TraitDefinition => {
              val traitType = td.typeSymbol.definedType.asInstanceOf[Types.Trait]
              val ctd = td.body.duplicate().transformSymbols { (st, sym) =>
                if (symTrans.contains(sym)) symTrans(sym)
                else {
                  traitType.symbolMember(sym) match {
                    case Some(m) => {
                      val ns = new IdSymbol(m.name, m.symbol.definition)
                      ns.ty = m.symbol.ty
                      symTrans += sym -> ns
                      ns
                    }
                    case None => sym
                  }
                }

              }

              val newStruct = sd.copy(body = Block(ctd.children.toList ++ sd.body.content))
              sd.copyAttrs(newStruct.asInstanceOf[sd.type])
              //Typer.typed(newStruct).asInstanceOf[StructDefinition]
              newStruct
            }
            case _ => Utils.error("Cannot extends from anything else than a trait (" + t.symbol.definition + ")")
          }
      }
        .transformSymbols { (st, sym) => symTrans.getOrElse(sym, sym) }
      println("End ST : " + symTrans)
      symbolTranslation += sd.typeSymbol -> symTrans
      processedStruct
    }
    case _ => s
  }
}