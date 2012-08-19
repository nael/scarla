package nasc
import java.io.FileWriter

class PrettyPrinter {
  var out: StringBuilder = null

  def withStyle(style: String, data: Map[String, String] = Map())(x: String): String =
    "<span class=\"" + style + "\" " + (data.map { case (k, v) => "data-" + k + "=\"" + v + "\" " }).mkString(" ") + "  >" + x + "</span>"
  def ident(s: String) = withStyle("identifier")(normIdent(s))
  def symbol(s: Symbol) = {
    var data = Map[String, String]()
    data ++= Map("uniqueName" -> s.uniqueName)
    if (s.typed) data ++= Map("type" -> s.typeSymbol.toString)
    withStyle("symbol", data)(s.name)
  }
  def literal = withStyle("literal") _
  def keyword = withStyle("keyword") _
  def typeName = withStyle("typeName") _
  def bracket = withStyle("bracket") _
  def decl = withStyle("decl") _

  def normIdent(s: String) = s

  def beginBlock() = out ++= "<div class=\"codeBlock\">"
  def endBlock() = out ++= "</div>"

  prelude()

  def prelude() = {
    out = new StringBuilder()
    out ++= """
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Compilation report</title>
<script type="text/javascript"
	src="http://code.jquery.com/jquery-1.7.2.min.js"></script>
      <link type="text/css" rel="stylesheet" href="jquery.qtip-2.0.0.css" />
      <script type="text/javascript" src="jquery.qtip-2.0.0.min.js"></script>
<script type="text/javascript" src="test.js"></script>
<STYLE TYPE="text/css">
.codeBlock {
	padding-left: 1.5em;
}
.identifier {
	color: black;
}
.symbol {
    color: black;
    font-weight: bold;
      text-decoration:underline
}
.typeName {
    color: #88F; //
}
.decl {
    font-weight: bold;      
}
.keyword {
	color: #500000;
    font-weight: bold;
}
.bracket {
   color: #800080;   
}
.code {
   background: #F0F0F0;
      font-family: monospace;
   display: none;
}
.symbolInfo {
   font-size: 1.1em;      
}
</STYLE>
</head>

<body>"""
  }

  def conclude() = {
    out ++= """</body>
</html>"""
  }

  def toFile(fn: String) = {
    conclude()
    val f = new FileWriter(fn)
    f.write(out.result())
    f.close()
    prelude()
  }

  def beginPhase(p: String) = {
    out ++= "<h3><a href='#' onclick='$(\"#" + p + "_code\").toggle(); return false'>[+]</a> <strong>" + p + "</strong></h3><div class='code' id='" + p + "_code'>"
  }

  def endPhase() = out ++= "</div><br/>"

  def mustPrint(x: Tree) = x match {
    case _ => true
  }

  def printSymbol(s: Symbol) = {
    out ++= symbol(s)
  }

  def printSymbolOr(s: Symbol, ss: String) = {
    if (s == null) out ++= ident(ss)
    else printSymbol(s)
  }

  def printAttrs(x: Tree) = {
    out ++= x.attrString
  }

  def prettyPrint(x: Tree): Unit = x match {
    case b: Block => {
      if (b.children.size == 0) {
        out ++= bracket("{") + "()" + bracket("}")
        /*} else if (b.children.size == 1) {
        prettyPrint(b.children.head)*/
      } else {
        out ++= bracket("{") + "<br/>"
        beginBlock()

        def printLines(ss: Seq[Tree]): Unit = {
          ss foreach { s =>
            if (mustPrint(s)) {
              s match {
                case b: Block => printLines(b.children)
                case _ => { prettyPrint(s); out ++= "<br/>" }
              }
            }
          }
        }

        printLines(b.children)
        endBlock()
        out ++= bracket("}")
      }
    }
    case n: Name => {
      out ++= n.name
    }
    case s: Sym => {
      out ++= symbol(s.symbol)
    }
    case lit: Literal => {
      out ++= literal(lit.value.toString)
    }
    case vd: ValDef => {
      out ++= keyword( /*if (mutable) */ "var" /* else "val"*/ )
      out ++= " "
      prettyPrint(vd.valName)
      out ++= " : "
      printTypeExpr(vd.typeTree)
      vd.value.foreach { value =>
        out ++= " = "
        prettyPrint(value)
      }
    }
    case d: DefDef => {
      printAttrs(d)
      out ++= keyword("def")
      out ++= " "
      prettyPrint(d.defName)
      out ++= "("
      prettyPrintSeq(d.arguments)
      out ++= ")"
      d.body foreach { t =>
        out ++= " = "
        prettyPrint(t)
      }
    }
    case a: ArgDef => {
      prettyPrint(a.argName)
      out ++= " : "
      printTypeExpr(a.typeTree)
    }
    case t: Trait => {
      out ++= keyword("trait")
      out ++= " "
      prettyPrint(t.body)
    }
    case s: Struct => {
      out ++= keyword("struct")
      out ++= "("
      prettyPrintSeq(s.arguments)
      out ++= ") "
      if (!s.composedTraits.isEmpty) {
        out ++= "< "
        prettyPrintSeq(s.composedTraits)
        out ++= " "
      }
      prettyPrint(s.content)
    }
    case td: TypeDef => {
      printAttrs(td)
      out ++= keyword("type")
      out ++= " "
      prettyPrint(td.typeName)
      out ++= "["
      prettyPrintSeq(td.typeVars)
      out ++= "]"
      td.value foreach { v =>
        out ++= " = "
        prettyPrint(v)
      }
    }
    case a: Assign => {
      prettyPrint(a.dest)
      out ++= " = "
      prettyPrint(a.value)
    }
    case i: If => {
      out ++= keyword("if")
      out ++= "("
      prettyPrint(i.condition)
      out ++= ") "
      prettyPrint(i.ifTrue)
      out ++= keyword(" else ")
      prettyPrint(i.ifFalse)
    }
    case a: Apply => {
      prettyPrint(a.function)
      out ++= "("
      prettyPrintSeq(a.arguments)
      out ++= ")"
    }
    case s: Select => {
      prettyPrint(s.from)
      out ++= "."
      prettyPrint(s.memberName)
    }
    case n: New => {
      out ++= keyword("new")
      out ++= " "
      prettyPrint(n.typeTree)
      out ++= "("
      prettyPrintSeq(n.args)
      out ++= ")"
    }
    /*case StructDefinition(name, args, traits, body, isVal) => {
      out ++= keyword(if (isVal) "struct" else "class")
      out ++= " " + name + " ("
      prettyPrintArgs(args)
      out ++= ") "
      prettyPrint(body)
    }
    case fd @ FunctionDefinition(name, args, body, retTe) => {
      out ++= keyword("def") + " "
      printSymbolOr(fd.funSymbol, name)
      out ++= "("
      prettyPrintArgs(args)
      out ++= ") : "
      printTypeExpr(retTe)
      out ++= " = "
      prettyPrint(body)
    }*/
    case w: While => {
      out ++= keyword("while") + "("
      prettyPrint(w.condition)
      out ++= ") "
      prettyPrint(w.body)
    }
    case bc: cast.BitCast => {
      out ++= keyword("bcast")
      out ++= "["
      printTypeExpr(bc.typeTree)
      out ++= "]("
      prettyPrint(bc.ptr)
      out ++= ")"
    }
    case uc: cast.UpCast => {
      out ++= keyword("ucast")
      out ++= "["
      printTypeExpr(uc.typeTree)
      out ++= "]("
      prettyPrint(uc.value)
      out ++= ")"
    }

    case _ => { if (mustPrint(x)) out ++= "[" + x.getClass() + "]" else () }
  }

  def printSeq[T](l: Seq[T])(f: T => Unit) = l match {
    case Seq() => ()
    case _ => f(l.reduce { (a1, a2) => f(a1); out ++= ", "; a2 })
  }

  def prettyPrintSeq(l: Seq[Tree]) = printSeq(l)(prettyPrint)

  def prettyPrintArgs(args: Seq[ArgDef]) = printSeq(args) { arg => prettyPrint(arg.argName); out ++= " : "; printTypeExpr(arg.typeTree) }

  def print(s: String) = out ++= s.replace("\n", "<br/>")

  def printTypeExpr(te: Tree): Unit = {
    te match {
      case n: Name => out ++= typeName(n.name)
      case s: Sym => out ++= symbol(s.symbol)
      case a: Apply => {
        printTypeExpr(a.function)
        out ++= "["
        printTypeExpr(a.arguments.reduce { (a1, a2) => printTypeExpr(a1); out ++= ", "; a2 })
        out ++= "]"
      }
    }
  }
} 