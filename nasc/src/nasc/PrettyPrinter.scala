/*package nasc
import java.io.FileWriter

class PrettyPrinter {
  var out: StringBuilder = null

  def withStyle(style: String, data : Map[String, String] = Map())(x: String): String =
    "<span class=\"" + style + "\" " + (data.map { case (k,v) => "data-" + k + "=\"" + v + "\" " }).mkString(" ") + "  >" + x + "</span>"
  def ident(s : String) = withStyle("identifier")(normIdent(s))
  def symbol(s : IdSymbol) = {
    var data = Map[String, String]()
    data ++= Map("uniqueName" -> s.uniqueName)
    if(s.typed) data ++= Map("type" -> s.ty.toString)
    withStyle("symbol", data)(s.name)
  }
  def literal = withStyle("literal") _
  def keyword = withStyle("keyword") _
  def typeName = withStyle("typeName") _
  def bracket = withStyle("bracket") _
  def decl = withStyle("decl") _
  
  def normIdent(s : String) = s
  
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
    case _: BuiltinFunDef => false
    case _: BuiltinTypeDef => false
    case _ => true
  }

  def printSymbol(s: IdSymbol) = {
    out ++= symbol(s)
  }

  def printSymbolOr(s: IdSymbol, ss: String) = {
    if (s == null) out ++= ident(ss)
    else printSymbol(s)
  }

  def prettyPrint(x: Tree): Unit = x match {
    case Block(Seq()) => out ++= bracket("{") + "()" + bracket("}")
    case Block(Seq(y)) => prettyPrint(y)
    case Block(xs) => {
      out ++= bracket("{") + "<br/>"
      beginBlock()
      xs.foreach { s => if (mustPrint(s)) { prettyPrint(s); out ++= "<br/>" } }
      endBlock()
      out ++= bracket("}")
    }
    case id : Id => {
      printSymbolOr(id.symbol, id.name)
    }
    case lit: Literal[_] => {
      out ++= literal(lit.value.toString)
    }
    case vd @ ValDefinition(name, typeExpr, valueOpt, mutable) => {
      out ++= keyword(if (mutable) "var" else "val")
      out ++= " "
      printSymbolOr(vd.valSymbol, name)
      out ++= " : "
      printTypeExpr(typeExpr)
      valueOpt.map { value =>
        out ++= " = "
        prettyPrint(value)
      }
    }
    case Assign(lv, v) => {
      prettyPrint(lv)
      out ++= " = "
      prettyPrint(v)
    }
    case If(cond, tb, fb) => {
      out ++= keyword("if")
      out ++= "("
      prettyPrint(cond)
      out ++= ") "
      prettyPrint(tb)
      out ++= keyword(" else ")
      prettyPrint(fb)
    }
    case Call(f, args) => {
      prettyPrint(f)
      out ++= "("
      prettyPrintSeq(args)
      out ++= ")"
    }
    case Select(e, f) => {
      prettyPrint(e)
      out ++= "."
      out ++= ident(f)
    }
    case ExternFunDef(name, llname, ty, redec) => {
      out ++= keyword("extern")
      out ++= " " + name + " : "
      out ++= ty.toString
    }
    case StructDefinition(name, args, traits, body, isVal) => {
      out ++= keyword(if(isVal) "struct" else "class")
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
    }
    case While(cond, body) => {
    	out ++= keyword("while") + "("
    	prettyPrint(cond)
    	out ++= ") "
    	prettyPrint(body)
    }
    
    case PtrRef(x) => {
      out ++= "&"
      prettyPrint(x)
    }
    
    case PtrDeref(x) => {
      out ++= "(*"
      prettyPrint(x)
      out ++= ")"
    }
    
    case _ => { if (mustPrint(x)) out ++= "[" + x.getClass() + "]" else () }
  }

  def printSeq[T](l: Seq[T])(f: T => Unit) = l match {
    case Seq() => ()
    case _ => f(l.reduce { (a1, a2) => f(a1); out ++= ", "; a2 })
  }

  def prettyPrintSeq(l: Seq[Tree]) = printSeq(l)(prettyPrint)

  def prettyPrintArgs(args: Seq[Definition.Argument]) = printSeq(args) { arg => printSymbolOr(arg.symbol, arg.name); out ++= " : "; printTypeExpr(arg.typeExpr) }

  def print(s: String) = out ++= s.replace("\n", "<br/>")

  def printTypeExpr(te: TypeExpr): Unit = {
    te match {
      case TypeId(name) => out ++= typeName(name)
      case TypeApply(name, args) => {
        out ++= typeName(name)
        out ++= "["
        printTypeExpr(args.reduce { (a1, a2) => printTypeExpr(a1); out ++= ", "; a2 })
      }
    }
  }
}*/ 