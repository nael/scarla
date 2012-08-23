package nasc

trait TypeInfo {
  def members: Seq[Symbol]
  def derived: Seq[Symbol] = Seq()

  def vals = members filter { m => m.definition != null && (m.definition.isInstanceOf[ValDef]) }
}

case class Conversion(from: Symbol, to: Symbol, conv: Tree => Tree)

object Glob {
  var conversions = Seq[Conversion]()
}

class TypePhase extends Phase[Tree, Tree] {

  def name = "type"
  def execute(t: Tree): Tree = {
    val typedTree = Typer.typeTree(t)
    Typer.check(typedTree)
    typedTree
  }
}

object Typer {
  val NoType = new Symbol {
    def name = "<undefined>"
    var typeSymbol: Symbol = null
    var isType = true
    var definition: Def = null

    typeInfo = new TypeInfo {
      def members = Seq()
    }
  }

  def typeTree(t: Tree): Tree = {
    val tt = t transform { case d: TypeDef => makeThis(d) } transform { case s: Struct => expandValCtor(s) }
    val typeDefs = tt collect { case d: TypeDef => d }
    populateTypeInfos(typeDefs)

    val typer = new TypeTransform()
    val typedTree = typer.transform(tt)
    typedTree
  }

  def expandValCtor(s: Struct): Struct = {
    var vds = Seq[Tree]()
    s.arguments filter { _.hasAttr[attributes.Val] } foreach { arg =>
      arg.attr -= attributes.Val()
      val argSym = new Symbol {
        val name = arg.argName.name
        var typeSymbol: Symbol = null
        var isType = false
        var definition: Def = arg
      }
      val vd = new ValDef(arg.argName, arg.typeTree, Some(new Sym(argSym)))
      arg.argName.symbol.definition = vd
      vds :+= vd
      arg.argName = new Sym(argSym)
    }
    s.content.asInstanceOf[Block].children ++= vds //TODO berk cast
    s
  }

  // Takes a type tree and return all possible overloaded type symbols
  def typeTreeSymbols(t: Tree): Seq[Symbol] = {
    t match {

      case s: Sym => s.symbols filter { _.isType }

      case a: Apply => {
        val fSyms = typeTreeSymbols(a.function)
        val typeValsO = a.arguments map typeTreeSymbols
        Utils.assert(typeValsO forall { _.size == 1 })
        val typeVals = typeValsO map { _.first }
        var syms = Seq[Symbol]()
        fSyms.foreach { s =>
          s.definition match {
            case td: TypeDef => {
              if (td.typeVars.size == typeVals.size) {
                val typeEnv = td.typeVars map { _.symbol } zip typeVals

                syms +:= s.derivedSymbols find { _.typeVars == typeEnv } getOrElse {
                  val newSym = new Symbol {
                    def name = {
                      val extractedLocalValue = Utils.repsep(typeVals map { _.toString })
                      s + "[" + extractedLocalValue + ("]")
                    }
                    var typeSymbol: Symbol = null
                    var isType = true
                    var definition: Def = s.definition
                    typeVars = typeEnv
                    typeInfo = s.typeInfo
                  }

                  s.derivedSymbols +:= newSym
                  newSym
                }
              }
            }
            case _ => ()
          }
        }
        syms
      }
    }
  }

  def typeTreeSymbol(t: Tree): Symbol = {
    val syms = typeTreeSymbols(t)
    if (syms.size != 1) Utils.error("Wrong od for " + t + " : " + syms)
    t.typeSymbol = NoType
    syms.head
  }

  def convert(tree: Tree, to: Symbol): Option[Tree] = {
    Utils.assert(tree.typed && to.isType)
    //    println("Trying conversion " + to + " -> " + tree.typeSymbol + " --- " + tree)
    if (tree.typeSymbol == to) Some(tree)
    else if (to == Builtin.Unit.symbol) {
      Some(Typer.typeTree(new Block(Seq(tree, new Literal(null)))))
    } else {
      Glob.conversions find { x => x.from == tree.typeSymbol && x.to == to } map { conv =>
        Typer.typeTree(conv.conv(tree))
      }
    }
  }

  class TypeTransform extends TreeTransform {
    val doTransform: PartialFunction[Tree, Tree] = {
      case s: Sym if s.symbol.typed => {
        s.typeSymbol = s.symbol.typeSymbol
        s
      }
      case s: Sym if !s.typed => {
        s.typeSymbol = s.symbol.definition match {
          case vd: ValDef => {
            typeTreeSymbol(vd.typeTree)
          }
          case ad: ArgDef => {
            typeTreeSymbol(ad.typeTree)
          }
          case dd: DefDef => {
            val ret = typeTreeSymbol(dd.returnTypeTree)
            val a = dd.arguments map { arg => typeTreeSymbol(arg.typeTree) }
            typeTreeSymbol(new Apply(new Sym(Builtin.Functions(a.size).symbol), a.map { new Sym(_) } :+ new Sym(ret)))
          }
          case td: TypeDef => {
            NoType
          }
          case _ => null
        }

        if (s.typed) // TODO Seems magic but maybe useful ?
          s.symbol.typeSymbol = s.typeSymbol
        s
      }

      case sel: Select if !sel.typed => {
        if (sel.from.typed) {
          val ty = sel.from.typeSymbol.typeInfo
          ty.members find { _.name == sel.memberName.name } match {
            case Some(member) => {
              sel.memberName = transform(new Sym(member))
              sel.typeSymbol = sel.memberName.typeSymbol
              sel
            }
            case None => sel
          } // TODO overloads ...
        } else { println("Couldnt type " + sel + " because " + sel.from + " is not typed"); sel }
      }

      case a: Assign if !a.typed => {
        if (a.dest.typed && a.value.typed) {
          convert(a.value, a.dest.typeSymbol) match {
            case Some(v) => {
              a.value = v
              a.typeSymbol = Builtin.Unit.symbol
            }
            case _ => ()
          }
          a
        } else a
      }

      case b: Block if !b.typed => {
        b.typeSymbol =
          if (b.children.isEmpty) Builtin.Unit.symbol
          else if (b.children.last.typed) b.children.last.typeSymbol
          else null
        b
      }

      case d: Def if !d.typed => {
        d match {
          case vd: ValDef if !vd.value.isEmpty => {
            val v = vd.value.get
            if (v.typed) {
              convert(v, typeTreeSymbol(vd.typeTree)) match {
                case None => ()
                case Some(convertedV) => {
                  vd.value = Some(convertedV)
                  vd.typeSymbol = Builtin.Unit.symbol
                }
              }
            }
          }
          // case DefDef TODO check return type
          case _ => {
            d.typeSymbol = Builtin.Unit.symbol
          }
        }
        d
      }

      case s: Struct if !s.typed => {
        s.typeSymbol = NoType; s
      }

      case t: Trait if !t.typed => {
        t.typeSymbol = NoType; t
      }

      case ta: cast.TypeAttr if !ta.typed => {
        if (ta.tree.typed) {
          val ty = ta.tree.typeSymbol
          val reqAttr = (ty.definition.attr -- ta.remove) ++ ta.add
          ty.derivedSymbols find { sym =>
            sym.typeVars == ty.typeVars && sym.definition.attr == reqAttr
          } match {
            case Some(s) => ta.typeSymbol = s
            case None => Utils.error("Removing qualifier on type never seen before, should create")
          }
          ta
        } else ta
      }

      case bc: cast.BitCast if !bc.typed => {
        if (bc.ptr.typed) {
          bc.typeSymbol = typeTreeSymbol(bc.typeTree)
          bc
        } else bc
      }

      case uc: cast.UpCast if !uc.typed => {
        if (uc.value.typed) {
          //TODO check if uc.value.typeSymbol implements uc.typeTree
          uc.typeSymbol = typeTreeSymbol(uc.typeTree)
          uc
        } else uc
      }

      case i: If if !i.typed => {
        if (i.condition.typed && i.ifTrue.typed && i.ifFalse.typed) {
          (convert(i.condition, Builtin.Boolean.symbol), convert(i.ifTrue, i.ifFalse.typeSymbol)) match { // TODO haveCommonConv
            case (Some(cond), Some(tb)) => {
              i.condition = cond
              i.ifTrue = tb
              i.typeSymbol = i.ifFalse.typeSymbol
            }
            case _ => ()
          }
          i
        } else i
      }

      case w: While if !w.typed => {
        if (w.condition.typed && w.body.typed) {
          convert(w.condition, Builtin.Boolean.symbol) match {
            case Some(cond) => {
              w.condition = cond
              w.typeSymbol = Builtin.Unit.symbol
              w
            }
            case _ => w
          }
        } else w
      }

      case l: Literal if !l.typed => {
        l.typeSymbol = l.value match {
          case i: Int => Builtin.Int.symbol
          case b: Boolean => Builtin.Boolean.symbol
          case null => Builtin.Unit.symbol // TODO ugh
          case _ => Utils.error("Unknown literal type : " + l.value.getClass)
        }
        l
      }

      case n: New if !n.typed => {
        if (n.typeTree.typed) {
          //TODO check ctor args correctness
          n.typeSymbol = typeTreeSymbol(n.typeTree)
          n
        } else n
      }

      case a: Apply if !a.typed => {
        if (a.function.typed && a.arguments.forall(_.typed)) {
          if (Builtin.isFunction(a.function.typeSymbol)) {
            val fArgTypes = Builtin.functionArgTypes(a.function.typeSymbol)
            if (fArgTypes.size != a.arguments.size) Utils.error("Wrong argument count for " + a.function + " got " + a.arguments.size + " expected " + fArgTypes.size)
            val args = (fArgTypes zip a.arguments) map { case (ty, arg) => convert(arg, ty) }
            if (args.contains(None)) Utils.error("Wrong arg type : " + fArgTypes + " / " + a.arguments)

            a.arguments = args map { _.get }
            a.typeSymbol = Builtin.functionReturnType(a.function.typeSymbol)

            a
          } else Utils.error("Not callable : " + a.function.typeSymbol)
        } else a
      }
      case t => t
    }
  }

  // TODO here linearization of types & ... for now it just proceed in order and can fail (horribly)
  // also verify concrete types do not lack any unimplemented members
  def populateTypeInfos(tds: Seq[TypeDef]) = {
    tds.reverse foreach { td =>
      val ts = typeTreeSymbol(td.typeName)
      td.value foreach {
        case st: Struct => {
          val m = st.content.children collect {
            case vd: ValDef => vd.valName.symbol
            case dd: DefDef => dd.defName.symbol
          } toSeq
          val composed = st.composedTraits flatMap { _.symbol.typeInfo.members } // Worst way possible
          ts.typeInfo = new TypeInfo {
            override val derived = composed filter { s => !(m map { _.name } contains s.name) } // again
            def members = m
          }

          st.composedTraits foreach { tr =>
            Glob.conversions +:= Conversion(ts, tr.symbol, { t =>
              new cast.UpCast(t, new Sym(tr.symbol))
            })
          }
        }
        case t: Trait => {
          val m = t.body.children collect {
            case vd: ValDef => vd.valName.symbol
            case dd: DefDef => dd.defName.symbol
          }
          ts.typeInfo = new TypeInfo {
            def members = m.toSeq
          }
        }
        case s: Sym => {
          Utils.assert(s.symbol.typeInfo != null)
          ts.typeInfo = s.symbol.typeInfo
          s.symbol.derivedSymbols +:= ts
          ts.derivedSymbols +:= s.symbol
        }
        case x => println("No typeinfo for " + x)
      }
      td.typeVars.foreach { tv => tv.typeSymbol = NoType; tv.symbol.typeSymbol = NoType }
      ts.typeSymbol = NoType
    }
  }

  def makeThis(td: TypeDef): Tree = {
    td.value match {
      case Some(st: Struct) => {
        val ts = typeTreeSymbol(td.typeName)
        if (td.hasAttr[attributes.Move] || td.hasAttr[attributes.CopyThis]) {
          st.thisTree.symbol.typeSymbol = ts
          td
        } else {
          ts.derivedSymbols find { sym =>
            sym.typeVars == ts.typeVars && sym.definition.hasAttr[attributes.Move]
          } match {
            case None => {
              val ptrSym = new Symbol { // TODO way too ugly, time for type Ptr[T] = move T ?
                def name = td.typeName.name + "Ptr"
                var typeSymbol: Symbol = null
                var isType = true
                var definition: Def = null
              }
              val ptrTd = new TypeDef(new Sym(ptrSym), Seq(), Some(new Sym(ts)))
              ptrSym.definition = ptrTd
              ptrTd.attr += attributes.Move()

              Glob.conversions +:= Conversion(ts, ptrSym, { t: Tree =>
                new cast.TypeAttr(Set(attributes.Move()), Set(), t)
              })
              st.thisTree.symbol.typeSymbol = ptrSym
              new Block(Seq(ptrTd, td))
            }
            case Some(sym) => {
              st.thisTree.symbol.typeSymbol = sym
              td
            }
          }

        }
      }
      case _ => td
    }
  }

  def check(t: Tree): Unit = {
    var ok = true
    val untyped = t filter { !_.typed } toSet

    if (!untyped.isEmpty) {
      println("Untyped trees : " + untyped.map(_.toString))
      ok = false
    } else {
      if (G.verbose) println("Whole tree is typed !")
    }

    val syms = (t collect { case x if x.hasSymbol => x.symbol }) ++ (t collect { case x if x.typed => x.typeSymbol })
    syms.toSet.foreach { sym: Symbol =>
      if (sym.isType) {
        if (sym.typeInfo == null) {
          println("No type info on " + sym)
          ok = false
        }
      } else {
        if (!sym.typed) {
          println("Untyped symbol : " + sym + " : " + sym.typeSymbol)
          ok = false
        }
      }
    }

    val failed = t collect { case s: Sym if s.typed && s.typeSymbol != s.symbol.typeSymbol => s }
    if (!failed.isEmpty) {
      println("Bad symbols : ")
      failed.toSet foreach { s: Sym =>
        println(s.symbol + " : " + s.typeSymbol + " != " + s.symbol.typeSymbol)
      }
      //ok = false//TODO XXXXXX
    }

    if (!ok) {
      println("++++++++++++++++++++++++++++++++++++++++++++++")
      println(t)
      Utils.error("Typing failed")
    }
  }

}
