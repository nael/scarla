package nasc

trait TreeAttribute {
  
}

object attributes {
  case class Native(nativeName: String) extends TreeAttribute
}

trait Tree {
  def children: Iterable[Tree]

  def hasSymbol = false
  def symbol: Symbol = Utils.error("No symbol in " + this)
  def symbol_=(x: Symbol): Unit = Utils.error("Cannot put symbol into " + this)

  var _typeSymbol: Symbol = null
  def typeSymbol = if (typed) _typeSymbol else Utils.error("No type in " + this)
  def typeSymbol_=(o: Symbol) = { _typeSymbol = o }
  def typed = _typeSymbol != null
  
  var attr : Set[TreeAttribute] = Set()

  def traverse(f: PartialFunction[Tree, Unit]): Unit = new TreeTraverse { val doTraverse = f } traverse this
  def collect[T](f: PartialFunction[Tree, Iterable[T]]): List[T] = {
    val collector = new TreeTraverse {
      var res = List[T]()
      val doTraverse = f andThen { x => res ++= x }
    }
    collector.traverse(this)
    collector.res
  }
  def filterPartial(f: PartialFunction[Tree, Boolean]): List[Tree] = collect { case t if f.isDefinedAt(t) && f(t) => List(t) }
  def filter(f: Tree => Boolean): List[Tree] = {
    f match {
      case pf: PartialFunction[Tree, Boolean] => filterPartial(pf)
      case _ => filterPartial { case x => f(x) }
    }
  }
}

trait Def extends Tree

class Apply(var function: Tree, var arguments: List[Tree]) extends Tree {
  def children = function :: arguments

  override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
}

class ArgDef(var argName: Tree, var typeTree: Tree) extends Def {
  def children = List(argName, typeTree)

  override def toString = argName.toString + " : " + typeTree.toString
}

class ValDef(var valName: Tree, var typeTree: Tree, var value: Option[Tree]) extends Def {
  def children = valName :: typeTree :: value.toList

  override def toString = "val " + valName + " : " + typeTree.toString + (value.map(" = " + _.toString).getOrElse(""))
}

class TypeDef(var typeName: Tree, var typeVars: Iterable[Tree], var value: Option[Tree]) extends Def {
  def children = typeName :: value.toList

  override def toString = "type " + typeName.toString + (value match { case None => "" case Some(v) => " = " + v.toString })
}

class DefDef(var defName: Tree, var arguments: List[ArgDef], var returnTypeTree: Tree, var body: Option[Tree]) extends Template with Def {
  override def children = (super.children :+ defName) ++ body.toList
  
  override def toString = "def " + defName.toString + "(" + Utils.repsep(arguments map {_.toString}) + ")" + (body map { " = " + _.toString} getOrElse "")
}

trait Template extends Tree {
  def arguments: List[ArgDef]
  def children: List[Tree] = arguments
}

class Struct(var arguments: List[ArgDef], var content: Tree) extends Template {
  override def children = super.children :+ content

  override def toString = "struct(" + Utils.repsep(arguments.map(_.toString)) + ") " + content.toString
}

object Builtin {
  case class Ty(name: String, tv: Iterable[Symbol] = List()) {
    var symbol: Symbol = null
  }
  val Int = Ty("Int")

  val Unit = Ty("Unit")

  def makeTypeVar(s: String): Symbol = {
    new Symbol {
      def name = s
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
    }
  }
  
  def functionReturnType(x: Symbol): Symbol = { Utils.assert(isFunction(x)); x.typeVars.last._2}
  def functionArgTypes(x: Symbol): List[Symbol] = { Utils.assert(isFunction(x)); x.typeVars.dropRight(1) map { _._2 } toList }
  def isFunction(x : Symbol): Boolean = Functions exists {_.symbol.derivedSymbols contains x}
  val Functions = (1 to 7) map { i =>
    Ty("Function", ((1 to i) map { j => makeTypeVar("Arg" + j) }) :+ makeTypeVar("Ret"))
  } 

  val typeDefs = List(Int, Unit) ++ Functions map makeDef

  def makeDef(ty: Ty): TypeDef = {
    val s = new Symbol {
      def name = ty.name
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
    }
    val d = new TypeDef(new Sym(s), ty.tv map { new Sym(_) }, None)
    s.definition = d
    ty.symbol = s
    d
  }
}

class Block(var children: List[Tree]) extends Tree {
  override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
}

class Literal[T](var value: T) extends Tree {
  def children = List()

  override def toString = "[lit:" + value.toString + "]"
}

class Name(val name: String, val isTypeName: Boolean) extends Tree {
  override def toString = name + "?"

  def children = List()
}
class Sym(var symbols: List[Symbol]) extends Tree {
  Utils.assert(!symbols.isEmpty)
  
  def this(s: Symbol) = { this(List(s)) }
  
  def children = List()
  override def hasSymbol = true
  override def symbol = symbols.head
  
  override def toString = if (symbol.isType) {
    "<" + symbol + (if(symbols.size > 1) "+" else "") +">"
  } else { "[" + symbol + " : " + (if (typed) typeSymbol else "?") + (if(symbols.size > 1) "+" else "") + "]" }
}

trait TreeTraverse {

  val doTraverse: PartialFunction[Tree, Unit]

  def traverse(t: Tree): Unit = {
    if (doTraverse.isDefinedAt(t)) doTraverse(t)
    t.children.foreach(traverse)
  }
}

trait TreeTransform {
  //val preTransform : PartialFunction[Tree, Tree]
  val doTransform: PartialFunction[Tree, Tree]
  def transformList(t: List[Tree]): List[Tree] = { t.map(transform) }
  def transform(t: Tree): Tree = {
    val tt = t //preTransform(t)
    tt match {
      case a: Apply => {
        a.function = transform(a.function)
        a.arguments = transformList(a.arguments)
      }
      case ad: ArgDef => {
        ad.argName = transform(ad.argName)
        ad.typeTree = transform(ad.typeTree)
      }
      case vd: ValDef => {
        vd.valName = transform(vd.valName)
        vd.typeTree = transform(vd.typeTree)
        vd.value = vd.value.map { v => transform(v) }
      }
      case td: TypeDef => {
        td.typeName = transform(td.typeName)
        td.value = td.value.map(transform)
      }
      case dd: DefDef => {
        dd.defName = transform(dd.defName)
        dd.arguments = transformList(dd.arguments).asInstanceOf[List[ArgDef]]
        dd.returnTypeTree = transform(dd.returnTypeTree)
        dd.body = dd.body map transform
      }
      case s: Struct => {
        s.arguments = transformList(s.arguments).asInstanceOf[List[ArgDef]]
        s.content = transform(s.content)
      }
      case b: Block => {
        b.children = transformList(b.children)
      }
      case _: Literal[_] => ()
      case _: Name => ()
      case _: Sym => ()
      //case x => x 
    }
    if (doTransform.isDefinedAt(tt)) doTransform(tt.asInstanceOf[Tree])
    else tt
  }

}

