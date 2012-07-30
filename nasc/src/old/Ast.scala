package nasc

import java.io.ByteArrayOutputStream
import java.io.PrintWriter

object G {
  var verbose = true
  //val pp = new PrettyPrinter()
}

class CompilationUnit(var root: Tree) {
  override def toString() = root.toString()
}

trait AttrCopy {
  def copyAttrs(that: this.type): Unit = {}
}
/*
trait Tree extends AttrCopy {
  def children: Seq[Tree]

  def subTypeExprs: Iterable[TypeExpr] = Seq()
  def scoped = false
  
  def typed = true

  def transform(f: Tree => Tree) =
    new AstTransformer {
      def transformSymbol(parent: Tree, s: Symbol) = s
      def transform(x: Tree): Tree = f(x)
    }.applyOn(this)

  def transformSymbols(f: (Tree, Symbol) => Symbol) =
    new AstTransformer {
      def transformSymbol(parent: Tree, s: Symbol) = f(parent, s)
      def transform(x: Tree): Tree = x
    }.applyOn(this)

  def duplicate() = transform(identity)
}

trait AstTransformer {
  def transformSymbol(parent: Tree, sym: Symbol): Symbol
  def transform(x: Tree): Tree
  def applyOnSymbols(s: Tree): Unit = s match {
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
      sel.fieldSymbol = transformSymbol(sel, sel.fieldSymbol).asInstanceOf[IdSymbol]
    }
    case id: Id => {
      id.symbol = transformSymbol(id, id.symbol).asInstanceOf[IdSymbol]
    }

  }
  def applyOn[T <: Tree](x: T): T = {
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


trait Expr extends Tree {

  override def typed = ty != null
  
  var ty: Type = null
  var typeSymbol: TypeSymbol = null

  override def copyAttrs(e: this.type): Unit = {
    super.copyAttrs(e)
    e.ty = ty
    e.typeSymbol = typeSymbol
  }
}


trait SymDef extends Tree {
  def symbols: Seq[Symbol] = Seq()
  override def typed = super.typed && symbols.forall(_.typed)
}

trait TypeSymDef[T <: Type] extends SymDef {
  var typeSymbol : TypeSymbol = null
  override def symbols = typeSymbol :: super.symbols
  def definedType : T = typeSymbol.definedType.asInstanceOf[T]
}

case class BuiltinTypeDef extends Tree with SymDef {
  def children = Seq()
  override def toString = "__builtins__(" + Utils.repsep(Defs.types.list.map(_.typeSymbol.toString)) + ")"
    
  override def symbols = typeSymbols ++ super.symbols

  var typeSymbols: Seq[TypeSymbol] = Defs.types.list.map { case t => t.typeSymbol }
  override def copyAttrs(e: this.type): Unit = {
    e.typeSymbols = typeSymbols
  }
}

case class ExternFunDef(name: String, llvmName: String, functionType: Defs.types.Function.Instance, redeclare: Boolean) extends Tree with SymDef {
  def children = Seq()

  def declareSymbols() = {}
  

  override def symbols = funSymbol :: super.symbols

  var funSymbol: IdSymbol = new IdSymbol(name, this)
  override def copyAttrs(e: this.type): Unit = {
    super.copyAttrs(e)
    e.funSymbol = funSymbol
  }

}

case class BuiltinFunDef(fun: BuiltinFunction) extends Tree with SymDef {
  def children = Seq()

  def declareSymbols() = {}

  
  override def symbols = funSymbol :: super.symbols

  var funSymbol: IdSymbol = new IdSymbol(fun.name, this)
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

case class TypeApply(name: String, args: Seq[TypeExpr]) extends TypeExpr {
  override def toString = (if (symbol == null) "?" + name else symbol.toString()) + "[" + Utils.repsep(args.map(_.toString)) + "]"
}

case class Block(content: Seq[Tree]) extends Expr {
  override def toString = "{\n" + Utils.repsep(content.map(_.toString), "\n") + "\n}"
  override def children = content
  override def scoped = true
}

trait Literal[+T] extends Expr {
  def value: T
  override def toString = "lit(" + value.toString + ")"
  def children = Seq()
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
  override def children = value match { case Some(x) => Seq(x) case None => Seq() }
  override def symbols = valSymbol :: super.symbols
  override def subTypeExprs = Seq(valTypeExpr)

  def declareSymbols() = {}
  

  var valSymbol: IdSymbol = new IdSymbol(name, this)

  override def copyAttrs(vd: this.type): Unit = {
    super.copyAttrs(vd)
    vd.valSymbol = valSymbol
  }
}

case class Assign(lvalue: LValue, value: Expr) extends Expr {
  override def toString = lvalue.toString + " = " + value.toString
  def children = Seq(lvalue, value)
}

case class If(condition: Expr, trueBlock: Expr, falseBlock: Expr) extends Expr {
  override def toString = "if(" + condition.toString() + ") " + trueBlock.toString() + " else " + falseBlock.toString()
  override def children = Seq(condition, trueBlock, falseBlock)
}

case class While(condition: Expr, body: Expr) extends Expr {
  def children = Seq(condition, body)

}

case class Return(value: Expr) extends Expr {
  override def toString = "return " + value.toString()
  override def children = Seq(value)
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

  case class Argument(name: String, typeExpr: TypeExpr, mode: ArgModes.Mode) extends Expr with SymDef {
    def children = Seq()
    var symbol: IdSymbol = new IdSymbol(name, this)
    override def symbols = Seq(symbol)
    override def toString = ArgModes.toString(mode) + " " + Utils.symbolOr(symbol, name + " : " + typeExpr)
    override def subTypeExprs = Seq(typeExpr)
  }
}

trait Definition extends Tree {
  def arguments: Seq[Definition.Argument] 
  def children : Seq[Tree] = arguments
}

case class FunctionDefinition(name: String, args: Seq[Definition.Argument], body: Expr, retTypeExpr: TypeExpr) extends Tree with Definition with SymDef {

  def arguments = args
  
  override def toString = "def " + Utils.symbolOr(funSymbol, name) + "(" + Utils.repsep(args.map { arg => arg.toString() }) + ") : " + (if (returnType != null) returnType else "?" + retTypeExpr) + " = " + body
  override def children = super.children ++ Seq(body)
  override def symbols = funSymbol :: super.symbols
  
  override def subTypeExprs = retTypeExpr :: args.map(_.typeExpr)
  override def scoped = true

  
  var funSymbol: IdSymbol = new IdSymbol(name, this)
  var returnType: Type = null

  def functionType: Defs.types.Function.Instance = Defs.types.Function.create(returnType, arguments.map(_.symbol.ty))
  
  override def copyAttrs(fd: this.type) = {
    super.copyAttrs(fd)
    super[Definition].copyAttrs(fd)
    fd.returnType = returnType
    fd.funSymbol = funSymbol
  }
  
  override def typed = super.typed && returnType != null
}

trait AggregateDefinition[T <: Type] extends Tree with TypeSymDef[T] {

  def body: Block
  def name: String

  override def scoped = true
  
  typeSymbol = new TypeSymbol(name, this)
  
  override def copyAttrs(d: this.type) = {
    d.typeSymbol = typeSymbol
  }

}

case class TraitDefinition(name: String, body: Block) extends AggregateDefinition[Types.Trait] {

  override def toString = "trait " + Utils.symbolOr(typeSymbol, "?" + name) + " = " + body.toString()
  def children = Seq(body)

  /*def copyAttrs(d: this.type) = {
    super[AggregateDefinition].copyAttrs(d)
  }*/

}

case class StructDefinition(name: String, constructorArguments: Seq[Definition.Argument], traits: Seq[TypeExpr], body: Block, isValue : Boolean) extends Tree
with SymDef with Definition with AggregateDefinition[Types.Struct] {
  def arguments = constructorArguments

  override def toString = "struct" + Utils.symbolOr(typeSymbol, name) + (if (traits.isEmpty) "" else " extends (" + Utils.repsep(traits.map(_.toString)) + ")") + " = " + body.toString()
  override def children = super.children ++ Seq(body)
  override def symbols = Seq(initSymbol, constructorSymbol, thisSymbol) ++ super.symbols
  override def subTypeExprs = traits
  

  var initSymbol: IdSymbol = new IdSymbol("this", this)
  var constructorSymbol: IdSymbol = new IdSymbol(name, this)
  var thisSymbol: IdSymbol = new IdSymbol("new", this)
  var vtableSymbols: Map[TypeSymbol, IdSymbol]  = traits.map({ t => t.symbol -> new IdSymbol("_" + t.symbol.uniqueName + "_vtable", this )}).toMap
  
  override def copyAttrs(sd: this.type) = {
    super.copyAttrs(sd)
    super[Definition].copyAttrs(sd)
    super[AggregateDefinition].copyAttrs(sd)
    sd.constructorSymbol = constructorSymbol
    sd.thisSymbol = thisSymbol
    sd.initSymbol = initSymbol
    sd.vtableSymbols = vtableSymbols
  }
}

case class QualId(ids: Seq[String]) extends Expr {
  override def children = Seq()
}

case class Call(f: Expr, args: Seq[Expr]) extends Expr {
  override def toString = {
    
    val sargs = args.map(_.toString)
    f.toString() + "(" + Utils.repsep(sargs) + ") : " + ty
  }
  override def children = f :: args
}

case class PtrRef(ptr: Expr) extends Expr {
  def children = Seq(ptr)
}

case class IRValue(name: String) extends Expr {
  override def toString = "llvm(" + name + ")"
  override def children = Seq()
}

case class Select(o: Expr, name: String) extends LValue {
  override def toString = "(" + o.toString() + "." + Utils.symbolOr(fieldSymbol, name) + " : " + ty + ")"
  override def children = Seq(o)
  
  override def typed = super.typed && fieldSymbol != null
  
  var fieldSymbol: IdSymbol = null

  override def copyAttrs(ma: this.type): Unit = {
    super.copyAttrs(ma)
    ma.fieldSymbol = fieldSymbol
  }
}

object Id {
  def fromSymbol(s: IdSymbol): Id = {
    val id = Id(s.uniqueName)
    id.symbol = s
    id.ty = s.ty
    id
  }
}

case class Id(name: String) extends LValue {
  override def toString = Utils.symbolOr(symbol, name)

  override def children = Seq()

  var symbol: IdSymbol = null

  override def copyAttrs(id: this.type): Unit = {
    super.copyAttrs(id)
    id.symbol = symbol
  }

}

case class PtrDeref(ptr: Expr) extends LValue {
  def children = Seq(ptr)
  override def toString = "(*" + ptr.toString + ") : " + ty
}
*/