package nasc

trait TreeAttribute

object attributes {
  case class Native(nativeName: String) extends TreeAttribute {
    override def toString = "native(" + nativeName + ")"
  }
  case class Heap extends TreeAttribute {
    override def toString = "heap"
  }
  case class Move extends TreeAttribute {
    override def toString = "move"
  }
}

trait Tree {
  def children: Iterable[Tree]

  def hasSymbol = false
  def symbol: Symbol = Utils.error("No symbol in " + this)
  def symbol_=(x: Symbol): Unit = Utils.error("Cannot put symbol into " + this)

  def name: String = Utils.error("No name")

  var _typeSymbol: Symbol = null
  def typeSymbol = if (typed) _typeSymbol else Utils.error("No type in " + this)
  def typeSymbol_=(o: Symbol) = { _typeSymbol = o }
  def typed = _typeSymbol != null
  
  def attrString = attr.toSeq.map(_.toString + " ") mkString ""

  var attr: Set[TreeAttribute] = Set()
  def hasAttr[T] = !attr.find({ x: TreeAttribute => x.isInstanceOf[T] }).isEmpty

  def traverse(f: PartialFunction[Tree, Unit]): Unit = new TreeTraverse { val doTraverse = f } traverse this
  def transform(f: PartialFunction[Tree, Tree]): Tree = new TreeTransform { val doTransform = f } transform this
  def transformExpr(f: PartialFunction[Tree, Tree]): Tree = new TreeTransform { exprOnly = true; val doTransform = f } transform this
  def collect[T](f: PartialFunction[Tree, T]): Seq[T] = {
    val collector = new TreeTraverse {
      var res = Seq[T]()
      val doTraverse = f andThen { x => res +:= x }
    }
    collector.traverse(this)
    collector.res
  }
  def filterPartial(f: PartialFunction[Tree, Boolean]): Seq[Tree] = collect { case t if f.isDefinedAt(t) && f(t) => t }
  def filter(f: Tree => Boolean): Seq[Tree] = {
    f match {
      case pf: PartialFunction[Tree, Boolean] => filterPartial(pf)
      case _ => filterPartial { case x => f(x) }
    }
  }
}

trait Def extends Tree

class New(var typeTree: Tree, var args: Seq[Tree]) extends Tree {
  def children = typeTree +: args
  override def toString = "new " + typeTree.toString
}

class Apply(var function: Tree, var arguments: Seq[Tree]) extends Tree {
  def children = function +: arguments

  override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
}

// Manual for now until better view
object cast {
  class TypeAttr(var add: Set[TreeAttribute], var remove: Set[TreeAttribute], var tree: Tree) extends Tree {
    def children = Seq(tree)
    
    override def toString = "cast[" + ((add map {"+" + _}) ++ (remove map {"-" + _})).mkString(" ") + "](" + tree.toString + ")" 
  }
}

class ArgDef(var argName: Tree, var typeTree: Tree) extends Def {
  def children = Seq(argName, typeTree)

  override def toString = argName.toString + " : " + typeTree.toString
}

class ValDef(var valName: Tree, var typeTree: Tree, var value: Option[Tree]) extends Def {
  def children = valName +: typeTree +: value.toSeq

  override def toString = "val " + valName + " : " + typeTree.toString + (value.map(" = " + _.toString).getOrElse(""))
}

class TypeDef(var typeName: Tree, var typeVars: Seq[Tree], var value: Option[Tree]) extends Def {
  def children = (typeName +: typeVars) ++ value.toSeq

  override def toString = attrString + "type " + typeName.toString + (value match { case None => "" case Some(v) => " = " + v.toString })
}

class DefDef(var defName: Tree, var arguments: Seq[ArgDef], var returnTypeTree: Tree, var body: Option[Tree]) extends Template with Def {
  override def children = (super.children :+ defName) ++ body.toSeq

  override def toString = attrString + "def " + defName.toString + "(" + Utils.repsep(arguments map { _.toString }) + ")" + (body map { " = " + _.toString } getOrElse "")
}

class Select(var from: Tree, var memberName: Tree) extends Tree {
  def children = Seq(from, memberName)

  override def toString = from.toString + "." + memberName.toString
}

class Assign(var dest: Tree, var value: Tree) extends Tree {
  def children = Seq(dest, value)

  override def toString = dest.toString + " = " + value.toString
}

trait Template extends Tree {
  def arguments: Seq[ArgDef]
  def children: Seq[Tree] = arguments
}

class Struct(var arguments: Seq[ArgDef], var content: Tree) extends Template {
  var thisTree: Tree = new Name("this", false)

  override def children = super.children :+ thisTree :+ content

  override def toString = "struct(" + Utils.repsep(arguments.map(_.toString)) + ") " + content.toString
}

object Builtin {
  case class Ty(name: String, ti: TypeInfo, tv: Seq[Symbol] = Seq()) {
    var symbol: Symbol = null
  }

  val Int = Ty("Int", new TypeInfo { def members = Seq() })

  val Unit = Ty("Unit", new TypeInfo { def members = Seq() })

  val Functions = (0 to 7) map { i =>
    Ty("Function",
      new TypeInfo { def members = Seq() },
      ((1 to i) map { j => makeTypeVar("Arg" + j) }) :+ makeTypeVar("Ret") toSeq) 
  }

  def makeTypeVar(s: String): Symbol = {
    new Symbol {
      def name = s
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
      typeInfo = new TypeInfo { def members = Seq() }
    }
  }

  // TODO move somewhere useful when in need
  def isTypeInstanceOf(x: Symbol, t: Symbol) = {
    Utils.assert(t.isType)
    (t.derivedSymbols contains x) &&
      (t.definition.asInstanceOf[TypeDef].typeVars forall { tv: Tree => x.typeVars exists { case (tv0, _) => tv.symbol == tv0 } })
  }
  def functionReturnType(x: Symbol): Symbol = { Utils.assert(isFunction(x)); x.typeVars.last._2 }
  def functionArgTypes(x: Symbol): Seq[Symbol] = { Utils.assert(isFunction(x)); x.typeVars.dropRight(1) map { _._2 } toSeq }
  def isFunction(x: Symbol): Boolean = Functions exists { f => isTypeInstanceOf(x, f.symbol) }

  val typeDefs = Seq(Int, Unit) ++ Functions map makeDef

  def makeDef(ty: Ty): TypeDef = {
    val s = new Symbol {
      def name = ty.name
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
      typeInfo = ty.ti
    }
    val d = new TypeDef(new Sym(s), ty.tv map { new Sym(_) }, None)
    s.definition = d
    ty.symbol = s
    d
  }
}

class Block(var children: Seq[Tree]) extends Tree {
  override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
}

class Literal[T](var value: T) extends Tree {
  def children = Seq()

  override def toString = "[lit:" + value.toString + "]"
}

class Name(override val name: String, val isTypeName: Boolean) extends Tree {
  override def toString = name + "?"

  def children = Seq()
}
class Sym(var symbols: Seq[Symbol]) extends Tree {
  Utils.assert(!symbols.isEmpty)

  def this(s: Symbol) = { this(Seq(s)) }

  def children = Seq()
  override def hasSymbol = true
  override def symbol = symbols.head
  override def name = symbol.name

  override def toString = if (symbol.isType) {
    "<" + symbol + (if (symbols.size > 1) "+" else "") + ">"
  } else { "[" + symbol + " : " + (if (typed) typeSymbol else "?") + (if (symbols.size > 1) "+" else "") + "]" }
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
  var exprOnly = false
  def transformSeq(t: Seq[Tree]): Seq[Tree] = { t.map(transform) }
  def transform(t: Tree): Tree = {
    val tt = t //preTransform(t)
    tt match {
      case a: Apply => {
        a.function = transform(a.function)
        a.arguments = transformSeq(a.arguments)
      }
      case ad: ArgDef => {
        if(!exprOnly) ad.argName = transform(ad.argName)
        if(!exprOnly) ad.typeTree = transform(ad.typeTree)
      }
      case vd: ValDef => {
        if(!exprOnly) vd.valName = transform(vd.valName)
        if(!exprOnly) vd.typeTree = transform(vd.typeTree)
        vd.value = vd.value.map { v => transform(v) }
      }
      case td: TypeDef => {
        if(!exprOnly) td.typeName = transform(td.typeName)
        td.value = td.value.map(transform)
      }
      case dd: DefDef => {
        if(!exprOnly) dd.defName = transform(dd.defName)
        if(!exprOnly) dd.arguments = transformSeq(dd.arguments).asInstanceOf[Seq[ArgDef]]
        if(!exprOnly) dd.returnTypeTree = transform(dd.returnTypeTree)
        dd.body = dd.body map transform
      }
      case s: Struct => {
        if(!exprOnly) s.arguments = transformSeq(s.arguments).asInstanceOf[Seq[ArgDef]]
        if(!exprOnly) s.thisTree = transform(s.thisTree)
        s.content = transform(s.content)
      }
      case b: Block => {
        b.children = transformSeq(b.children)
      }
      case s: Select => {
        s.from = transform(s.from)
        if(!exprOnly) s.memberName = transform(s.memberName)
      }
      case a: Assign => {
        a.dest = transform(a.dest)
        a.value = transform(a.value)
      }
      case n: New => {
        if(!exprOnly) n.args = transformSeq(n.args)
        if(!exprOnly) n.typeTree = transform(n.typeTree)
      }
      case ta: cast.TypeAttr => {
        ta.tree = transform(ta.tree)
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

