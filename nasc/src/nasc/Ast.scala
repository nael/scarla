package nasc

import java.io.ByteArrayOutputStream
import java.io.PrintWriter

object G {

  val pp = new PrettyPrinter()
}

class CompilationUnit(var root: Statement) {
  override def toString() = root.toString()
}

trait AttrCopy {
  def copyAttrs(that: this.type): Unit = {}
}

trait Statement extends AttrCopy {
  val uniq = { Expr.UNIQ += 1; Expr.UNIQ } //TODO berk
  def children: Iterable[Statement]

  def subTypeExprs: Iterable[TypeExpr] = List()
  def scoped = false
  
  def typed = true

  def transform(f: Statement => Statement) =
    new AstTransformer {
      def transformSymbol(parent: Statement, s: Symbol) = s
      def transform(x: Statement): Statement = f(x)
    }.applyOn(this)

  def transformSymbols(f: (Statement, Symbol) => Symbol) =
    new AstTransformer {
      def transformSymbol(parent: Statement, s: Symbol) = f(parent, s)
      def transform(x: Statement): Statement = x
    }.applyOn(this)

  def duplicate() = transform(identity)
}

trait AstTransformer {
  def transformSymbol(parent: Statement, sym: Symbol): Symbol
  def transform(x: Statement): Statement
  def applyOnSymbols(s: Statement): Unit = s match {
    case _: Block => ()
    case _: If => ()
    case _: While => ()
    case _: Call => ()
    case _: PtrRef => ()
    case _: PtrDeref => ()
    case _: Assign => ()
    case _: Literal[_] => ()

    case _: BuiltinFunDef => () //TODO ??
    case _: BuiltinTypeDef => ()
    case _: ExternFunDef => ()

    case d: ValDefinition => {
      d.valSymbol = transformSymbol(d, d.valSymbol).asInstanceOf[IdSymbol]
      d.typeSymbol = transformSymbol(d, d.typeSymbol).asInstanceOf[TypeSymbol]
    }
    case d: FunctionDefinition => {
      d.funSymbol = transformSymbol(d, d.funSymbol).asInstanceOf[IdSymbol]
    }
    case d: StructDefinition => {
      d.constructorSymbol = transformSymbol(d, d.constructorSymbol).asInstanceOf[IdSymbol]
      d.thisSymbol = transformSymbol(d, d.thisSymbol).asInstanceOf[IdSymbol]
      d.typeSymbol = transformSymbol(d, d.typeSymbol).asInstanceOf[TypeSymbol]
    }
    case d: TraitDefinition => {
      d.typeSymbol = transformSymbol(d, d.typeSymbol).asInstanceOf[TypeSymbol]
    }
    case sel: Select => {
      sel.fieldSymbol = transformSymbol(sel, sel.fieldSymbol)
    }
    case id: Id => {
      id.symbol = transformSymbol(id, id.symbol)
    }

  }
  def applyOn[T <: Statement](x: T): T = {
    val xformed = (x match {
      case Block(xs) => (Block(xs.map(applyOn)))
      case ValDefinition(name, te, v, mut) => (ValDefinition(name, te, v.map(applyOn), mut))
      case Assign(lv, v) => (Assign(applyOn(lv), applyOn(v)))
      case If(cond, tb, fb) => (If(applyOn(cond), applyOn(tb), applyOn(fb)))
      case While(cond, b) => (While(applyOn(cond), applyOn(b)))
      case FunctionDefinition(name, args, body, retTypeExpr) =>
        (FunctionDefinition(name, args, applyOn(body), retTypeExpr))
      case StructDefinition(name, ctorArgs, traits, body, v) => (StructDefinition(name, ctorArgs, traits, applyOn(body), v))
      case TraitDefinition(name, body) => TraitDefinition(name, applyOn(body))
      case Call(f, args) => (Call(applyOn(f), args.map(applyOn)))
      case PtrRef(e) => (PtrRef(applyOn(e)))
      case PtrDeref(e) => (PtrDeref(applyOn(e)))
      case Select(e, name) => (Select(applyOn(e), name))
      case _ => {
        if (x.children.size != 0) Utils.error("Forgot something " + x) //TODO remove
        (x)
      }
    }).asInstanceOf[x.type]
    x.copyAttrs(xformed)
    applyOnSymbols(xformed)
    transform(xformed).asInstanceOf[T]
  }
}

object Expr { var UNIQ = 0 }

trait Expr extends Statement {

  override def typed = ty != null
  
  var ty: Type = null
  var typeSymbol: TypeSymbol = null

  override def copyAttrs(e: this.type): Unit = {
    super.copyAttrs(e)
    e.ty = ty
    e.typeSymbol = typeSymbol
  }
}

trait SymDef extends Statement {
  def symbols: Iterable[Symbol]
  def declareSymbols(): Unit
  override def typed = super.typed && symbols.forall(_.typed)
}

case class BuiltinTypeDef extends Statement with SymDef {
  def children = List()
  override def toString = "__builtins__(" + Utils.repsep(Defs.types.list.map(_.typeSymbol.toString)) + ")"
  
  def declareSymbols() {
    typeSymbols = Defs.types.list.map { case t => t.typeSymbol }
  }
  def symbols = typeSymbols

  var typeSymbols: List[TypeSymbol] = List()
  override def copyAttrs(e: this.type): Unit = {
    e.typeSymbols = typeSymbols
  }
}

case class ExternFunDef(name: String, llvmName: String, functionType: Defs.types.Function.Instance, redeclare: Boolean) extends Statement with SymDef {
  def children = List()

  def declareSymbols() = {
    funSymbol = new IdSymbol(name, this)
  }

  def symbols = List(funSymbol)

  var funSymbol: IdSymbol = null
  override def copyAttrs(e: this.type): Unit = {
    super.copyAttrs(e)
    e.funSymbol = funSymbol
  }

}

case class BuiltinFunDef(fun: BuiltinFunction) extends Statement with SymDef {
  def children = List()

  def declareSymbols() = {
    funSymbol = new IdSymbol(fun.name, this)

  }
  def symbols = List(funSymbol)

  var funSymbol: IdSymbol = null
  override def copyAttrs(e: this.type): Unit = {
    super.copyAttrs(e)
    e.funSymbol = funSymbol
  }
}

trait TypeExpr {
  var symbol: TypeSymbol = null
}

object TypeId {
  def fromSymbol(ts: TypeSymbol): TypeId = {
    val tid = TypeId(ts.name)
    tid.symbol = ts
    tid
  }
}

case class TypeId(name: String) extends TypeExpr {

  override def toString = if (symbol == null) "?" + name else symbol.toString()
}

case class TypeApply(name: String, args: List[TypeExpr]) extends TypeExpr {
  override def toString = (if (symbol == null) "?" + name else symbol.toString()) + "[" + Utils.repsep(args.map(_.toString)) + "]"
}

case class Block(content: List[Statement]) extends Expr {
  override def toString = "{\n" + Utils.repsep(content.map(_.toString), "\n") + "\n}"
  override def children = content
  override def scoped = true
}

trait Literal[+T] extends Expr {
  def value: T
  override def toString = "lit(" + value.toString + ")"
  def children = List()
}

object Literals {

  case class Integer(value: Int) extends Literal[Int] {
    ty = Defs.types.Int
  }

  case class Boolean(value: scala.Boolean) extends Literal[scala.Boolean] {
    ty = Defs.types.Bool
  }

  case class String(value: java.lang.String) extends Literal[java.lang.String] {
    ty = null
  }

}

trait LValue extends Expr

case class ValDefinition(name: String, valTypeExpr: TypeExpr, value: Option[Expr], mutable: Boolean) extends Expr with SymDef {
  override def toString = "val " + Utils.symbolOr(valSymbol, "?" + name + " : " + valTypeExpr) + (value match { case Some(v) => " = " + v.toString case _ => "" })
  override def children = value match { case Some(x) => List(x) case None => List() }
  def symbols = List(valSymbol)
  override def subTypeExprs = List(valTypeExpr)

  def declareSymbols() = {
    valSymbol = new IdSymbol(name, this)
  }

  var valSymbol: IdSymbol = null

  override def copyAttrs(vd: this.type): Unit = {
    super.copyAttrs(vd)
    vd.valSymbol = valSymbol
  }
}

case class Assign(lvalue: LValue, value: Expr) extends Expr {
  override def toString = lvalue.toString + " = " + value.toString
  def children = List(lvalue, value)
}

case class If(condition: Expr, trueBlock: Expr, falseBlock: Expr) extends Expr {
  override def toString = "if(" + condition.toString() + ") " + trueBlock.toString() + " else " + falseBlock.toString()
  override def children = List(condition, trueBlock, falseBlock)
}

case class While(condition: Expr, body: Expr) extends Expr {
  def children = List(condition, body)

}

case class Return(value: Expr) extends Expr {
  override def toString = "return " + value.toString()
  override def children = List(value)
}

object FunctionDefinition {

}

object Definition {
  object ArgModes extends Enumeration {
    type Mode = Value
    val Copy, Ref, Readonly = Value
    def default = Copy
    def toString(v: Value) = v match {
      case Copy => "copy"
      case Ref => "ref"
      case Readonly => "ro"
    }
  }

  case class Argument(name: String, typeExpr: TypeExpr, mode: ArgModes.Mode) {
    var parentDef: Definition = null // TODO fugly rework with sym rework
    var position: Int = -1
    var symbol: IdSymbol = null
    override def toString = ArgModes.toString(mode) + " " + Utils.symbolOr(symbol, name + " : " + typeExpr)
  }
}

trait Definition extends Statement with SymDef {
  def definitionArguments: Iterable[Definition.Argument]
  def argumentSymbols = definitionArguments.map(_.symbol).toList
  def declareArgumentSymbols() = {
    definitionArguments.foreach { a => a.symbol = new IdSymbol(a.name, this) }
  }
  def registerArgs() = {
    definitionArguments.zipWithIndex.foreach { case (arg, idx) => arg.parentDef = this; arg.position = idx }
  }
  override def copyAttrs(d: this.type) = {
    d.definitionArguments.zip(definitionArguments).foreach {
      case (na, oa) =>
        na.symbol = oa.symbol
    }
  }
}

case class FunctionDefinition(name: String, args: List[Definition.Argument], body: Expr, retTypeExpr: TypeExpr) extends Statement with Definition {

  def definitionArguments = args
  registerArgs()

  override def toString = "def " + Utils.symbolOr(funSymbol, name) + "(" + Utils.repsep(args.map { arg => arg.toString() }) + ") : " + (if (returnType != null) returnType else "?" + retTypeExpr) + " = " + body
  override def children = List(body)
  def symbols = funSymbol :: argumentSymbols

  override def subTypeExprs = retTypeExpr :: args.map(_.typeExpr)
  override def scoped = true

  def declareSymbols() = {
    declareArgumentSymbols()
    funSymbol = new IdSymbol(name, this)
  }

  var funSymbol: IdSymbol = null
  var returnType: Type = null
  def functionType: Defs.types.Function.Instance = Defs.types.Function.create(returnType, definitionArguments.map(_.symbol.ty))
  override def copyAttrs(fd: this.type) = {
    super.copyAttrs(fd)
    super[Definition].copyAttrs(fd)
    fd.returnType = returnType
    fd.funSymbol = funSymbol
  }
  
  override def typed = super.typed && returnType != null
}

trait AggregateDefinition extends Statement with SymDef {

  def body: Block
  def name: String

  override def scoped = true

  def declareSymbols() = {
    typeSymbol = new TypeSymbol(name, this)
  }

  var typeSymbol: TypeSymbol = null

  override def copyAttrs(d: this.type) = {
    d.typeSymbol = typeSymbol
  }

}

case class TraitDefinition(name: String, body: Block) extends AggregateDefinition {

  override def toString = "trait " + Utils.symbolOr(typeSymbol, "?" + name) + " = " + body.toString()
  def symbols = List(typeSymbol)
  def children = List(body)

  /*def copyAttrs(d: this.type) = {
    super[AggregateDefinition].copyAttrs(d)
  }*/

}

case class StructDefinition(name: String, constructorArguments: List[Definition.Argument], traits: List[TypeExpr], body: Block, isValue : Boolean) extends Statement with SymDef with Definition with AggregateDefinition {
  def definitionArguments = constructorArguments
  registerArgs()

  override def toString = "struct" + Utils.symbolOr(typeSymbol, name) + (if (traits.isEmpty) "" else " extends (" + Utils.repsep(traits.map(_.toString)) + ")") + " = " + body.toString()
  def children = List(body)
  def symbols = initSymbol :: typeSymbol :: constructorSymbol :: thisSymbol :: argumentSymbols
  override def subTypeExprs = traits ++ definitionArguments.map(_.typeExpr)

  override def declareSymbols() = {
    super[AggregateDefinition].declareSymbols()
    declareArgumentSymbols()
    thisSymbol = new IdSymbol("this", this)
    constructorSymbol = new IdSymbol(name, this)
    initSymbol = new IdSymbol("$init", this)
    vtableSymbols = traits.map({ t => t.symbol -> new IdSymbol("_" + t.symbol.uniqueName + "_vtable", this )}).toMap
  }

  var initSymbol: IdSymbol = null
  var constructorSymbol: IdSymbol = null
  var thisSymbol: IdSymbol = null
  var vtableSymbols: Map[TypeSymbol, IdSymbol]  = null
  override def copyAttrs(sd: this.type) = {
    super.copyAttrs(sd)
    super[Definition].copyAttrs(sd)
    super[AggregateDefinition].copyAttrs(sd)
    sd.constructorSymbol = constructorSymbol
    sd.thisSymbol = thisSymbol
    sd.typeSymbol = typeSymbol
    sd.initSymbol = initSymbol
    sd.vtableSymbols = vtableSymbols
  }
}

case class QualId(ids: List[String]) extends Expr {
  override def children = List()
}

case class Call(f: Expr, args: List[Expr]) extends Expr {
  override def toString = {
    val sargs = args.map(_.toString)
    f.toString() + "(" + Utils.repsep(sargs) + ") : " + ty
  }
  override def children = f :: args
}

case class PtrRef(ptr: Expr) extends Expr {
  def children = List(ptr)
}

case class IRValue(name: String) extends Expr {
  override def toString = "llvm(" + name + ")"
  override def children = List()
}

case class Select(o: Expr, name: String) extends LValue {
  override def toString = "(" + o.toString() + "." + Utils.symbolOr(fieldSymbol, name) + " : " + ty + ")"
  override def children = List(o)
  
  override def typed = super.typed && fieldSymbol != null
  
  var fieldSymbol: Symbol = null

  override def copyAttrs(ma: this.type): Unit = {
    super.copyAttrs(ma)
    ma.fieldSymbol = fieldSymbol
  }
}

object Id {
  def fromSymbol(s: Symbol): Id = {
    val id = Id(s.uniqueName)
    id.symbol = s
    id.ty = s.ty
    id
  }
}

case class Id(name: String) extends LValue {
  override def toString = Utils.symbolOr(symbol, name)

  override def children = List()

  var symbol: Symbol = null

  override def copyAttrs(id: this.type): Unit = {
    super.copyAttrs(id)
    id.symbol = symbol
  }

}

case class PtrDeref(ptr: Expr) extends LValue {
  def children = List(ptr)
  override def toString = "(*" + ptr.toString + ") : " + ty
}
