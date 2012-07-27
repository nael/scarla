package nasc;

import scala.util.parsing.combinator._

object Grammar extends RegexParsers {

  //import ast.generic.t._
  import ast.syntax._
  
  val id = """[a-zA-Z]([a-zA-Z0-9_])*"""r
  val integerLiteral = """[0-9]+"""r
  val booleanLiteral = """true|false"""
  val operators = """\+|-|\*"""r

  override protected val whiteSpace = """( |\t)+""".r

    def program: Parser[Tree] = expr // TODO maybe not that good
    
      def funTypeExpr : Parser[TypeExpr] = ("(" ~> repsep(typeExpr, ",") <~ ")"/* | (typeExpr ^^ { List(_)})*/) ~ "=>" ~ typeExpr ^^ { case args ~ _ ~ ret => TypeApply("Function", ret :: args)}
    def optValue: Parser[Option[Tree]] = (("=" ~ expr)?) ^^ { u => u.map { case _ ~ e => e } }

  def typeExpr: Parser[TypeExpr] = (
    id ~ "[" ~ repsep(typeExpr, ",") ~ "]" ^^ { case functor ~ _ ~ args ~ _ => TypeApply(functor, args.toList) }
    | funTypeExpr
    | id ^^ { s => TypeId(s) })
    
  def valDef = ("val" | "var") ~ id ~ ":" ~ typeExpr ~ optValue ^^ {
    case "val" ~ vn ~ _ ~ ty ~ value =>
      new ValDef(new Name(vn), ty, value)
    //case "var" ~ valName ~ _ ~ ty ~ value => t.ValDef(t.Id(valName), ty, value) //TODO mutability
  }
  def stmt = (/*funDef
    | */valDef
    //| structDef
    //| traitDef
    | expr)
  def blockSep = (";" | "\n")
  def block = ("{" ~> (blockSep*) ~> repsep(stmt, blockSep+) <~ (blockSep*) <~ "}") ^^ { x=> new Block(x) }    
    def lvalue: Parser[Tree] = (
    /*("*" ~ lvalue ^^ { case _ ~ e => PtrDeref(e) })
    | ((id ^^ { Id(_) }) | atom) ~ "." ~ memberAccess ^^ { case t ~ _ ~ i => i(t) }

    | */(id ^^ { x=> new Name(x) }))
  def expr: Parser[Tree] = (
    block
   // | lvalue ~ "=" ~ expr ^^ { case lv ~ _ ~ value => Assign(lv, value) }
   // | "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb ~ _ ~ fb => If(cond, tb, fb) }
    //| "if" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb => If(cond, tb, Block()) }
    //| "while" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ body => While(cond, body) }
    //| basicExpr ~ "==" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("=="), List(e1, e2)) }
    //| basicExpr ~ "<=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("<="), List(e1, e2)) }
    //| basicExpr ~ "!=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("!="), List(e1, e2)) }
    | basicExpr)
def argList = repsep(expr, ',')
  def basicExpr: Parser[Tree] = (

    (factorExpr ~ "+" ~ factorExpr) ^^ { case ta ~ _ ~ e => new Apply(new Name("+"), List(ta, e)) }
    | (factorExpr ~ "-" ~ factorExpr) ^^ { case ta ~ _ ~ e => new Apply(new Name("-"), List(ta, e)) }
    | factorExpr)
  def factorExpr: Parser[Tree] = (
    callExpr ~ "*" ~ callExpr ^^ { case e1 ~ _ ~ e2 => new Apply(new Name("*"), List(e1, e2)) }
    | callExpr)
  def callExpr: Parser[Tree] = (
    (lvExpr ~ "(" ~ argList ~ ")") ^^ { case e ~ _ ~ args ~ _ => new Apply(e, args) }
    | lvExpr)
  def lvExpr: Parser[Tree] = (
    lvalue | atom)
  def atom: Parser[Tree] = (
   /* "@".r ~ id ^^ { case _ ~ id => PtrRef(Id(id)) }
    |*/ literal | ("(" ~ expr ~ ")") ^^ { case _ ~ e ~ _ => e })
  
 def literal : Parser[Literal[Any]]= (
   (("-".r?) ~ integerLiteral) ^^ { case Some(_) ~ d => new Literal[Any](-Integer.parseInt(d)) case None ~ d => new Literal[Any](Integer.parseInt(d)) }
    | (""""[^"]*""""r) ^^ { s => new Literal[Any](s.slice(1, s.length() - 1)) })
    //| ("true"r) ^^ { _ => t.Literal[Boolean](true) }
    //| ("false"r) ^^ { _ => t.Literal[Boolean](false) })
/*
  def qualId = id ~ (("." ~> id)*) ^^ { case i ~ Nil => QualId(List(i)) case i ~ is => QualId(i :: is) }

  def program: Parser[Tree] = expr // TODO maybe not that good

  def funTypeExpr : Parser[TypeExpr] = ("(" ~> repsep(typeExpr, ",") <~ ")"/* | (typeExpr ^^ { List(_)})*/) ~ "=>" ~ typeExpr ^^ { case args ~ _ ~ ret => TypeApply("Function", ret :: args)}
  
  def typeExpr: Parser[TypeExpr] = (
    id ~ "[" ~ repsep(typeExpr, ",") ~ "]" ^^ { case functor ~ _ ~ args ~ _ => TypeApply(functor, args.toList) }
    | funTypeExpr
    | id ^^ { s => TypeId(s) })

  // Function definition
  def argMode = ("ref" ^^ { _ => Definition.ArgModes.Ref })
  def arg = (argMode?) ~ id ~ ":" ~ typeExpr ^^ {
    case Some(mode) ~ name ~ _ ~ ty => (mode, name, ty)
    case None ~ name ~ _ ~ ty => (Definition.ArgModes.default, name, ty)
  }
  def funArgList = repsep(arg, ",") ^^ { argList => argList.map { arg => Definition.Argument(arg._2, arg._3, arg._1) } }
  def funDef = "def" ~ (id | operators) ~ "(" ~ funArgList ~ ")" ~ ":" ~ typeExpr ~ "=" ~ funBody ^^ { case _ ~ name ~ _ ~ args ~ _ ~ _ ~ retType ~ _ ~ body => FunctionDefinition(name, args, body, retType) }
  def funBody = expr

  def optValue: Parser[Option[Expr]] = (("=" ~ expr)?) ^^ { u => u.map { case _ ~ e => e } }

  def valDef = ("val" | "var") ~ id ~ ":" ~ typeExpr ~ optValue ^^ {
    case "val" ~ valName ~ _ ~ ty ~ value => ValDefinition(valName, ty, value, false)
    case "var" ~ valName ~ _ ~ ty ~ value => ValDefinition(valName, ty, value, true)
  }

  // Expressions  
  def memberAccess: Parser[Expr => LValue] = qualId ^^ { q =>
    { e: Expr =>
      (q.ids.foldLeft(e.asInstanceOf[LValue]) { (b, a) => Select(b, a) }): LValue
    }
  }
  def argList = repsep(expr, ',')
  def optArgList = (("(" ~> funArgList <~ ")")?) ^^ { case None => List() case Some(x) => x }
  def traitDef: Parser[TraitDefinition] = "trait" ~ id ~ block ^^ { case _ ~ traitName ~ body => TraitDefinition(traitName, body)}
  def optExtends: Parser[List[TypeExpr]] = (("extends" ~ repsep(typeExpr, "with"))?) ^^ { case None => List() case Some(_ ~ tr) => tr} 
  def structDef: Parser[StructDefinition] =
    ("value" ^^ { _ => true } | "" ^^ { _ => false } ) ~ "class" ~ id ~ optArgList ~ optExtends ~ block ^^ { case isValue ~_ ~ structName ~ ctorArgs ~ traits ~ body => StructDefinition(structName, ctorArgs, traits, body, isValue) }
  def stmt = (funDef
    | valDef
    | structDef
    | traitDef
    | expr)
  def blockSep = (";" | "\n")
  def block = ("{" ~> (blockSep*) ~> repsep(stmt, blockSep+) <~ (blockSep*) <~ "}") ^^ { Block(_) }
  def lvalue: Parser[LValue] = (
    ("*" ~ lvalue ^^ { case _ ~ e => PtrDeref(e) })
    | ((id ^^ { Id(_) }) | atom) ~ "." ~ memberAccess ^^ { case t ~ _ ~ i => i(t) }

    | (id ^^ { Id(_) }))
  def expr: Parser[Expr] = (
    block
    | lvalue ~ "=" ~ expr ^^ { case lv ~ _ ~ value => Assign(lv, value) }
    | "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb ~ _ ~ fb => If(cond, tb, fb) }
    //| "if" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb => If(cond, tb, Block()) }
    | "while" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ body => While(cond, body) }
    | basicExpr ~ "==" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("=="), List(e1, e2)) }
    | basicExpr ~ "<=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("<="), List(e1, e2)) }
    | basicExpr ~ "!=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("!="), List(e1, e2)) }
    | basicExpr)

  def basicExpr: Parser[Expr] = (

    (factorExpr ~ "+" ~ factorExpr) ^^ { case t ~ _ ~ e => Call(Id("+"), List(t, e)) }
    | (factorExpr ~ "-" ~ factorExpr) ^^ { case t ~ _ ~ e => Call(Id("-"), List(t, e)) }
    | factorExpr)
  def factorExpr: Parser[Expr] = (

    callExpr ~ "*" ~ callExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("*"), List(e1, e2)) }
    | callExpr)
  def callExpr: Parser[Expr] = (

    (lvExpr ~ "(" ~ argList ~ ")") ^^ { case e ~ _ ~ args ~ _ => Call(e, args) }
    | lvExpr)
  def lvExpr: Parser[Expr] = (
    lvalue | atom)
  def atom: Parser[Expr] = (
    "@".r ~ id ^^ { case _ ~ id => PtrRef(Id(id)) }
    | literal | ("(" ~ expr ~ ")") ^^ { case _ ~ e ~ _ => e })
*/
}
