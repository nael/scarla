package nasc

class LiftMethodsPhase extends Phase[Tree, Tree] {

  def name = "lift-methods"

  def execute(t: Tree): Tree = {
    val res = (t transform extractMethods) transform flattenApplication
    Typer.typeTree(res) // TODO normally not necessary if not in debug
    Typer.check(res)
    res
  }

  val flattenApplication: PartialFunction[Tree, Tree] = {
    case app: Apply => {
      app.function match {
        case sel: Select => {
          sel.from.typeSymbol.definition match {
            case td: TypeDef if td.value exists { _.isInstanceOf[Struct] } => {
              sel.memberName traverse { case t => t.typeSymbol = null }
              app.function = sel.memberName
              app.arguments ::= sel.from
              app.typeSymbol = null
              Typer.typeTree(app)
            }
            case _ => app
          }
        }
        case _ => app
      }
    }
  }

  def extractStructMethods(s: Struct): (Tree, List[DefDef]) = {
    val (methods, rest) = s.content.children partition { _.isInstanceOf[DefDef] }
    s.content = Typer.typeTree(new Block(rest.toList))
    (s, methods.toList map { _.asInstanceOf[DefDef] })
  }

  def addArgument(thisS: Symbol)(d: DefDef): DefDef = {
    val argSym = new Symbol {
      def name = "this"
      var typeSymbol: Symbol = null
      var isType = false
      var definition: Def = null
    }
    val argDef = new ArgDef(new Sym(argSym), new Sym(thisS.typeSymbol))
    d.arguments ::= argDef
    argSym.definition = argDef
    d.typeSymbol = null
    d.defName.typeSymbol = null
    d.defName.symbol.typeSymbol = null
    Typer.typeTree(d)
    d transform {
      case s: Sym => {
        val nsym = s.symbols map { sym => if (sym == thisS) argSym else sym }
        if (nsym != s.symbols) {
          val ss = Typer.typeTree(new Sym(nsym))
          println(s + " => " + ss.typeSymbol + " <> " + s.typeSymbol)
          Utils.assert(ss.typeSymbol == s.typeSymbol)
          ss
        } else s
      }
    }
    d
  }

  val extractMethods: PartialFunction[Tree, Tree] = {
    case td: TypeDef => {
      td.value match {
        case Some(s: Struct) => {
          val (st, methods) = extractStructMethods(s)
          td.value = Some(st)
          if (!methods.isEmpty) {
            val lifted = methods map addArgument(s.thisTree.symbol)
            Typer.typeTree(new Block(td :: lifted))
          } else td
        }
        case _ => td
      }
    }
  }

}