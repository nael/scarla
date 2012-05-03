package nasc
import java.io.FileWriter

class PrettyPrinter {
  var out: StringBuilder = null

  def withStyle(style: String)(x: String): String = "<span class=\"" + style + "\">" + x + "</span>"
  def ident = withStyle("identifier") _
  def literal = withStyle("literal") _
  def keyword = withStyle("keyword") _
  def typeName = withStyle("typeName") _
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
<script type="text/javascript" src="test.js"></script>
<STYLE TYPE="text/css">
.codeBlock {
	padding-left: 1em;
}
.identifier {
	color: blue;
	font-weight: bold;
}
.keyword {
	color: red;
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
    out ++= "<h3>Phase <strong>" + p + "</strong></h3>"
  }
  
  def endPhase() = out ++ "<br/>"

  def prettyPrint(x: Statement): Unit = x match {
    case Block(List()) => out ++= "{()}"
    case Block(List(y)) => prettyPrint(y)
    case Block(xs) => {
      out ++= "{<br/>"
      beginBlock()
      xs.foreach { s => prettyPrint(s); out ++= "<br/>" }
      endBlock()
      out ++= "<br/>}<br/>"
    }
    case Id(u) => {
      out ++= ident(u)
    }
    case lit: Literal[_] => {
      out ++= literal(lit.value.toString)
    }
    case ValDefinition(name, typeExpr, valueOpt, mutable) => {
      out ++= keyword(if (mutable) "var" else "val")
      out ++= " "
      out ++= ident(name)
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
      if (!args.isEmpty) args.reduce { (a1, a2) => prettyPrint(a1); out ++= ", "; a2 }
      out ++= ")"
    }
    case While(cond, body) => {

    }
    case _ => ()
  }

  def print(s: String) = out ++= s.replace("\n", "<br/>")

  def printTypeExpr(te: TypeExpr): Unit = {
    te match {
      case TypeId(name) => out ++= typeName(name)
      case TypeApply(name, args) => {
        out ++= typeName(name)
        out ++= "["
        args.reduce { (a1, a2) => printTypeExpr(a1); out ++= ", "; a2 }
      }
    }
  }
}