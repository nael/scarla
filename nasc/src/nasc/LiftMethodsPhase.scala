package nasc

class LiftMethodsPhase extends Phase[Tree, Tree] {

  def name = "lift-methods"

  val CTOR_NAME = "$ctor"
  var constructors = Map[Symbol, Symbol]()

  def execute(t: Tree): Tree = {
    val res =
      ((t transform extractMethods) transform flattenApplication) transform alloc
    Typer.typeTree(res) // TODO normally not necessary if not in debug
    Typer.check(res)
    res
  }

  val flattenApplication: PartialFunction[Tree, Tree] = {
    case app: Apply => {
      app.function match {
        case sel: Select => {
          sel.from.typeSymbol.definition match {
            case td: TypeDef if td.value exists { v => v.isInstanceOf[Struct] } => {
              if (sel.memberName.symbol.definition.isInstanceOf[DefDef]) {
                sel.memberName traverse { case t => t.typeSymbol = null }
                app.function = sel.memberName
                app.arguments +:= sel.from
                app.typeSymbol = null
                println("Before typing : " + app)
                Typer.typeTree(app)
              } else app
            }
            case _ => app
          }
        }
        case _ => app
      }
    }
  }

  val alloc: PartialFunction[Tree, Tree] = {
    case n: New => {
      // Transforms (new A(a0,a1,...)) into { val $ = new A; A_$ctor($, a0,a1,...); $ }
      constructors get n.typeSymbol match {
        case Some(ctSym) => {
          val tmpSym = new Symbol {
            def name = "_"
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = null
          }
          val tmpDef = new ValDef(new Sym(tmpSym), new Sym(n.typeSymbol), Some(n))
          tmpSym.definition = tmpDef
          val finalBlock = new Block(Seq(
            tmpDef,
            new Apply(new Sym(ctSym), new Sym(tmpSym) +: n.args),
            new Sym(tmpSym)
          ))
          n.args = Seq()
          Typer.typeTree(finalBlock)
        }
        case None => Utils.error("No constructor ?")
      }
    }
  }

  // Remove method defs from the struct and returns (struct with only val members, method defs)
  // It also explicits the constructor in the process
  def extractStructMethods(s: Struct, ts: Symbol): (Tree, Seq[DefDef]) = {
    val (methods, rest) = s.content.children partition { _.isInstanceOf[DefDef] }
    val ctorBody = rest flatMap {
      case _: DefDef => Seq()
      case vd: ValDef if !vd.value.isEmpty => Seq(Typer.typeTree(new Assign(vd.valName, vd.value.get)))
      case _: ValDef => Seq()
      case t => Seq(t)
    } toSeq
    val ctorSym = new Symbol {
      def name = CTOR_NAME
      var typeSymbol: Symbol = null
      var isType = false
      var definition: Def = null
    }
    val ctor = new DefDef(new Sym(ctorSym), s.arguments, new Sym(Builtin.Unit.symbol), Some(new Block(ctorBody)))
    ctorSym.definition = ctor
    rest foreach { case vd: ValDef => vd.value = None case _ => () }
    s.content = Typer.typeTree(new Block(rest.toSeq))
    constructors += ts -> ctorSym
    (s, (Typer.typeTree(ctor) +: methods.toSeq) map { _.asInstanceOf[DefDef] })
  }

  def addArgument(thisS: Symbol)(d: DefDef): DefDef = {
    val argSym = new Symbol {
      def name = "this"
      var typeSymbol: Symbol = null
      var isType = false
      var definition: Def = null
    }
    val argDef = new ArgDef(new Sym(argSym), new Sym(thisS.typeSymbol))
    d.arguments +:= argDef
    argSym.definition = argDef

    d.typeSymbol = null
    d.defName.typeSymbol = null
    d.defName.symbol.typeSymbol = null

    Typer.typeTree(d)
    // Change all references to $this in the method body to match the newly created argument symbol
    d transform {
      case s: Sym => {
        val nsym = s.symbols map { sym => if (sym == thisS) argSym else sym }
        if (nsym != s.symbols) {
          val ss = Typer.typeTree(new Sym(nsym))
          Utils.assert(ss.typeSymbol == s.typeSymbol)
          ss
        } else s
      }
    }

    // Wrap all accesses to members of the struct into a $this._
    d.body = d.body.map {
      _ transformExpr {
        case s: Sym => {
          if (thisS.typeSymbol.typeInfo.members contains s.symbol) {
            Typer.typeTree(new Select(new Sym(argSym), s))
          } else s
        }
      }
    }
    d
  }

  val extractMethods: PartialFunction[Tree, Tree] = {
    case td: TypeDef => {
      td.value match {
        case Some(s: Struct) => {
          val (st, methods) = extractStructMethods(s, td.typeName.symbol)
          td.value = Some(st)
          if (!methods.isEmpty) {
            val lifted = methods map addArgument(s.thisTree.symbol)
            Typer.typeTree(new Block(td +: lifted))
          } else td
        }
        case _ => td
      }
    }
  }

}