/*package nasc

class TraitPhase extends Phase[CompilationUnit, CompilationUnit] {

  def name = "trait"

  def execute(cu: CompilationUnit): CompilationUnit = {
    //cu.root = cu.root.transform(insertTraitMembers)
    //cu.root = cu.root.transform(rerouteCalls)
    //cu.root = cu.root.transform(initVtable)
    //cu.root = cu.root.transform()
    //cu.root = cu.root.transform(virtualCalls)
    //cu.root = cu.root.transformSymbols { (_, s) => virtualSymbolTranslation.getOrElse(s, s) }
    cu
  }

  def initVtable(st: Tree): Tree = st match {
    case sd: StructDefinition => {
      val symTrans = symbolTranslation(sd.typeSymbol)
      val vtableInit = sd.traits.flatMap { tr =>
        val td = tr.symbol.definition.asInstanceOf[TraitDefinition]
        val vtable = ValDefinition("__vtable_" + td.typeSymbol.uniqueName, TypeId.fromSymbol(td.typeSymbol), None, true)
       // vtable.valSymbol = sd.vtableSymbols(tr.symbol)
        Seq(vtable)
        /*val vtableDef = vtable :: symTrans.foldLeft(Seq[Statement]()) {
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
      Typer.typeTree(newStruct)
      newStruct
    }
    case _ => st
  }

  var virtualSymbolTranslation: Map[Symbol, IdSymbol] = Map()

  def virtualCalls(s: Tree): Tree = s match {
    case c @ Call(sel @ Select(e, name), args) if virtualSymbolTranslation.contains(sel.fieldSymbol) => {
      sel.fieldSymbol = virtualSymbolTranslation(sel.fieldSymbol)
      sel.ty = sel.fieldSymbol.ty
      val call = Call(sel, e :: args)
      call.ty = c.ty
      call
    }
    case _ => s
  }

  def insertVtable(s: Tree): Tree = s match {
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
            Seq(vd)
          }
          case _ => Seq()
        })
      val vtableDef = StructDefinition(td.name + "__vtable", Seq(), Seq(), vtableBody, true)
      Typer.typeTree(vtableDef)

      val vd1 = ValDefinition("ptr", TypeId.fromSymbol(Defs.types.Ptr.create(Defs.types.Byte).typeSymbol), None, true)
      vd1.declareSymbols()
      val vd2 = ValDefinition("vt", TypeId.fromSymbol(Defs.types.Ptr.create(vtableDef.typeSymbol.definedType).typeSymbol), None, true)
      vd2.declareSymbols()
      val structBody = Block(Seq(
        vd1,
        vd2))
      val structDef = StructDefinition(td.name, Seq(), Seq(), structBody, true)
      val defs = Typer.typed(Block(Seq(structDef, vtableDef)))
      td.typeSymbol.replaceBy(structDef.typeSymbol)
      defs
    }
    case _ => s
  }

  var symbolTranslation: Map[TypeSymbol, Map[Symbol, IdSymbol]] = Map()

  def rerouteCalls(s: Tree): Tree = s match {
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

  def insertTraitMembers(s: Tree): Tree = s match {
    case sd: StructDefinition => {
      var symTrans = Map[Symbol, IdSymbol]()
      val processedStruct = sd.traits.foldLeft(sd) {
        case (sd, t) =>
          t.symbol.definition match {
            case td: TraitDefinition => {
              val traitType = td.definedType
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

              val newStruct = sd.copy(body = Block(ctd.children.toSeq ++ sd.body.content))
              sd.copyAttrs(newStruct.asInstanceOf[sd.type])
              //Typer.typed(newStruct).asInstanceOf[StructDefinition]
              newStruct
            }
            case _ => Utils.error("Cannot extends from anything else than a trait (" + t.symbol.definition + ")")
          }
      }
        .transformSymbols { (st, sym) => symTrans.getOrElse(sym, sym) }
      symbolTranslation += sd.typeSymbol -> symTrans
      processedStruct.asInstanceOf[StructDefinition].typeSymbol.definedType = null
      val x = Typer.typed(processedStruct)
      x
    }
    case _ => s
  }
}*/