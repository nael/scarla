package nasc;

import scala.util.parsing.combinator._

object Grammar extends RegexParsers {

  val id = """[a-zA-Z]([a-zA-Z0-9_])*"""r
  val integerLiteral = """[0-9]+"""r
  val booleanLiteral = """true|false"""
  val operators = """\+|-|\*|==|<="""r

  override protected val whiteSpace = """( |\t|\r)+""".r

  def program: Parser[Tree] = expr // TODO maybe not that good

  def funTypeExpr: Parser[Tree] = ("(" ~> repsep(typeExpr, ",") <~ ")" /* | (typeExpr ^^ { Seq(_)})*/ ) ~ "=>" ~ typeExpr ^^ { case args ~ _ ~ ret => new Apply(new Name("Function", true), ret :: args) }
  def optValue: Parser[Option[Tree]] = (("=" ~ expr)?) ^^ { u => u.map { case _ ~ e => e } }

  def typeExpr: Parser[Tree] = (
    id ~ "[" ~ repsep(typeExpr, ",") ~ "]" ^^ { case functor ~ _ ~ args ~ _ => new Apply(new Name(functor, true), args.toSeq) }
    | funTypeExpr
    | id ^^ { s => new Name(s, true) })

  def valDef = ("val" | "var") ~ id ~ ":" ~ typeExpr ~ optValue ^^ {
    case _ ~ vn ~ _ ~ ty ~ value =>
      new ValDef(new Name(vn, false), ty, value)
    //case "var" ~ valName ~ _ ~ ty ~ value => t.ValDef(t.Id(valName), ty, value) //TODO mutability
  }

  def structDef: Parser[Tree] =
    ("value" ^^ { _ => true } | "" ^^ { _ => false }) ~ "class" ~ id ~ optArgSeq /*~ optExtends */ ~ block ^^
      {
        case isValue ~ _ ~ structName ~ ctorArgs /*~ traits*/ ~ body => {
          val td = new TypeDef(new Name(structName, true), Seq(), Some(new Struct(ctorArgs, body)))
          if (!isValue) {
            td.attr += attributes.Heap()
            td.attr += attributes.Move()
          }
          td
        }
      }

  def argSeq = repsep(expr, ',') ^^ { _ toSeq }
  def arg = id ~ ":" ~ typeExpr ^^ {
    case name ~ _ ~ ty => (new Name(name, false), ty)
  }
  def funArgSeq = repsep(arg, ",") ^^ { argSeq => argSeq.map { arg => new ArgDef(arg._1, arg._2) } }

  def optArgSeq = (("(" ~> funArgSeq <~ ")")?) ^^ { case None => Seq() case Some(x) => x }
  def funDef = "def" ~ (id | operators) ~ "(" ~ funArgSeq ~ ")" ~ ":" ~ typeExpr ~ (("=" ~ expr)?) ^^
    {
      case _ ~ name ~ _ ~ args ~ _ ~ _ ~ retType ~ Some(_ ~ body) =>
        new DefDef(new Name(name, false), args, retType, Some(body))
      case _ ~ name ~ _ ~ args ~ _ ~ _ ~ retType ~ None =>
        new DefDef(new Name(name, false), args, retType, None)
    }

  def stmt = ((("native(" ~ id ~ ")")?) ~ funDef ^^ { case None ~ d => d case Some(_ ~ nid ~ _) ~ d => { d.attr += attributes.Native(nid); d } }
    | valDef
    | structDef
    //| traitDef
    | expr)
  def blockSep = (";" | "\n" | whiteSpace)
  def block = ("{" ~> (blockSep*) ~> repsep(stmt, blockSep+) <~ (blockSep*) <~ "}") ^^ { x => new Block(x) }

  def qualId = id ~ (("." ~> id)*) ^^ { case i ~ Nil => Seq(i) case i ~ is => i :: is }
  def memberAccess: Parser[Tree => Tree] = qualId ^^ { q =>
    { e: Tree =>
      (q.foldLeft(e) { (b, a) => new Select(b, new Name(a, false)) }): Tree
    }
  }

  def lvalue: Parser[Tree] = (
    ((id ^^ { new Name(_, false) }) | atom) ~ "." ~ memberAccess ^^ { case t ~ _ ~ i => i(t) }

    | (id ^^ { x => new Name(x, false) }))
  def expr: Parser[Tree] = (
    block
    | lvalue ~ "=" ~ expr ^^ { case lv ~ _ ~ value => new Assign(lv, value) }
    | "if" ~ "(" ~ expr ~ ")" ~ expr ~ "else" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb ~ _ ~ fb => new If(cond, tb, fb) }
    | "if" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ tb => new If(cond, tb, new Block(Seq())) }
    | "while" ~ "(" ~ expr ~ ")" ~ expr ^^ { case _ ~ _ ~ cond ~ _ ~ body => new While(cond, body) }
    | basicExpr ~ "==" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => new Apply(new Name("==", false), Seq(e1, e2)) }
    | basicExpr ~ "<=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => new Apply(new Name("<=", false), Seq(e1, e2)) }
    //| basicExpr ~ "!=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("!="), Seq(e1, e2)) }
    | basicExpr)
  def basicExpr: Parser[Tree] = (

    (factorExpr ~ "+" ~ factorExpr) ^^ { case ta ~ _ ~ e => new Apply(new Name("+", false), Seq(ta, e)) }
    | (factorExpr ~ "-" ~ factorExpr) ^^ { case ta ~ _ ~ e => new Apply(new Name("-", false), Seq(ta, e)) }
    | factorExpr)
  def factorExpr: Parser[Tree] = (
    callExpr ~ "*" ~ callExpr ^^ { case e1 ~ _ ~ e2 => new Apply(new Name("*", false), Seq(e1, e2)) }
    | callExpr)
  def callExpr: Parser[Tree] = (
    (lvExpr ~ "(" ~ argSeq ~ ")") ^^ { case e ~ _ ~ args ~ _ => new Apply(e, args) }
    | lvExpr)
  def lvExpr: Parser[Tree] = (
      "new " ~ id ~ (("(" ~ argSeq ~ ")")?) ^^ {case _ ~ cname ~ args  => new New(new Name(cname, true), args map { case _ ~ x ~ _ => x} getOrElse(Seq()))} |
     ("true"r) ^^ { _ => new Literal(true) }
  | ("false"r) ^^ { _ => new Literal(false) } | lvalue | atom)
  def atom: Parser[Tree] = (
    /* "@".r ~ id ^^ { case _ ~ id => PtrRef(Id(id)) }
    |*/ literal | ("(" ~ expr ~ ")") ^^ { case _ ~ e ~ _ => e })
  def literal: Parser[Tree] = (
    (("-".r?) ~ integerLiteral) ^^ { case Some(_) ~ d => new Literal(-Integer.parseInt(d)) case None ~ d => new Literal(Integer.parseInt(d)) }
    | (""""[^"]*""""r) ^^ { s => new Literal(s.slice(1, s.length() - 1)) }
  )
  /*
  def qualId = id ~ (("." ~> id)*) ^^ { case i ~ Nil => QualId(Seq(i)) case i ~ is => QualId(i :: is) }

  def program: Parser[Tree] = expr // TODO maybe not that good

  def funTypeExpr : Parser[TypeExpr] = ("(" ~> repsep(typeExpr, ",") <~ ")"/* | (typeExpr ^^ { Seq(_)})*/) ~ "=>" ~ typeExpr ^^ { case args ~ _ ~ ret => TypeApply("Function", ret :: args)}
  
  def typeExpr: Parser[TypeExpr] = (
    id ~ "[" ~ repsep(typeExpr, ",") ~ "]" ^^ { case functor ~ _ ~ args ~ _ => TypeApply(functor, args.toSeq) }
    | funTypeExpr
    | id ^^ { s => TypeId(s) })

  // Function definition
  def argMode = ("ref" ^^ { _ => Definition.ArgModes.Ref })
  def arg = (argMode?) ~ id ~ ":" ~ typeExpr ^^ {
    case Some(mode) ~ name ~ _ ~ ty => (mode, name, ty)
    case None ~ name ~ _ ~ ty => (Definition.ArgModes.default, name, ty)
  }
  def funArgSeq = repsep(arg, ",") ^^ { argSeq => argSeq.map { arg => Definition.Argument(arg._2, arg._3, arg._1) } }
  def funDef = "def" ~ (id | operators) ~ "(" ~ funArgSeq ~ ")" ~ ":" ~ typeExpr ~ "=" ~ funBody ^^ { case _ ~ name ~ _ ~ args ~ _ ~ _ ~ retType ~ _ ~ body => FunctionDefinition(name, args, body, retType) }
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
  def argSeq = repsep(expr, ',')
  def optArgSeq = (("(" ~> funArgSeq <~ ")")?) ^^ { case None => Seq() case Some(x) => x }
  def traitDef: Parser[TraitDefinition] = "trait" ~ id ~ block ^^ { case _ ~ traitName ~ body => TraitDefinition(traitName, body)}
  def optExtends: Parser[Seq[TypeExpr]] = (("extends" ~ repsep(typeExpr, "with"))?) ^^ { case None => Seq() case Some(_ ~ tr) => tr} 
  def structDef: Parser[StructDefinition] =
    ("value" ^^ { _ => true } | "" ^^ { _ => false } ) ~ "class" ~ id ~ optArgSeq ~ optExtends ~ block ^^ { case isValue ~_ ~ structName ~ ctorArgs ~ traits ~ body => StructDefinition(structName, ctorArgs, traits, body, isValue) }
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
    | basicExpr ~ "==" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("=="), Seq(e1, e2)) }
    | basicExpr ~ "<=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("<="), Seq(e1, e2)) }
    | basicExpr ~ "!=" ~ basicExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("!="), Seq(e1, e2)) }
    | basicExpr)

  def basicExpr: Parser[Expr] = (

    (factorExpr ~ "+" ~ factorExpr) ^^ { case t ~ _ ~ e => Call(Id("+"), Seq(t, e)) }
    | (factorExpr ~ "-" ~ factorExpr) ^^ { case t ~ _ ~ e => Call(Id("-"), Seq(t, e)) }
    | factorExpr)
  def factorExpr: Parser[Expr] = (

    callExpr ~ "*" ~ callExpr ^^ { case e1 ~ _ ~ e2 => Call(Id("*"), Seq(e1, e2)) }
    | callExpr)
  def callExpr: Parser[Expr] = (

    (lvExpr ~ "(" ~ argSeq ~ ")") ^^ { case e ~ _ ~ args ~ _ => Call(e, args) }
    | lvExpr)
  def lvExpr: Parser[Expr] = (
    lvalue | atom)
  def atom: Parser[Expr] = (
    "@".r ~ id ^^ { case _ ~ id => PtrRef(Id(id)) }
    | literal | ("(" ~ expr ~ ")") ^^ { case _ ~ e ~ _ => e })
*/
}
