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
  case class Val extends TreeAttribute {
    override def toString = "val"
  }
  case class CopyThis extends TreeAttribute {
    override def toString = "copy_this"
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
  override def toString = "new " + typeTree.toString + "(" + (args map { _.toString } mkString ", ") + ")"
}

class Apply(var function: Tree, var arguments: Seq[Tree]) extends Tree {
  def children = function +: arguments

  override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
}

// Manual for now until better view
object cast {
  class TypeAttr(var add: Set[TreeAttribute], var remove: Set[TreeAttribute], var tree: Tree) extends Tree {
    def children = Seq(tree)

    override def toString = "cast[" + ((add map { "+" + _ }) ++ (remove map { "-" + _ })).mkString(" ") + "](" + tree.toString + ")"
  }

  class BitCast(var ptr: Tree, var typeTree: Tree) extends Tree {
    def children = Seq(ptr, typeTree)

    override def toString = "cast[*" + typeTree.toString + "](" + ptr.toString + ")"
  }
  
  class UpCast(var value: Tree, var typeTree: Tree) extends Tree {
    def children = Seq(value, typeTree)
    
    override def toString = "cast[^" + typeTree.toString + "](" + value.toString + ")"
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

class ObjectDef(var objName: Tree, var body: Tree) extends Def {
  def children = Seq(objName, body)

  override def toString = "object " + objName.toString + " " + body.toString
}

trait Template extends Tree {
  def arguments: Seq[ArgDef]
  def children: Seq[Tree] = arguments
}

class Struct(var arguments: Seq[ArgDef], var composedTraits: Seq[Tree], var content: Tree) extends Template {
  var thisTree: Tree = new Sym(new Symbol {
    def name = "this"
    var typeSymbol: Symbol = null
    var isType = false
    var definition: Def = null
  })

  override def children = super.children ++ composedTraits :+ thisTree :+ content

  override def toString = "struct(" + Utils.repsep(arguments.map(_.toString)) + ") < " + (composedTraits map {_.toString} mkString ",") + content.toString
}

object Builtin {
  case class Ty(name: String, ti: TypeInfo, tv: Seq[Symbol] = Seq(), attrs: Set[TreeAttribute] = Set()) {
    var symbol: Symbol = null
  }

  val Int = Ty("Int", new TypeInfo {
    def members = Seq()
    def isDerived(s: Symbol) = false
  })
  val Boolean = Ty("Boolean", new TypeInfo {
    def members = Seq()
    def isDerived(s: Symbol) = false
  })
  val Unit = Ty("Unit", new TypeInfo {
    def members = Seq()
    def isDerived(s: Symbol) = false
  })
  val CPtr = Ty("CPtr", new TypeInfo {
    def members = Seq()
    def isDerived(s: Symbol) = false
  })

  val Functions = (0 to 7) map { i =>
    Ty("Function",
      new TypeInfo {
        def members = Seq()
        def isDerived(s: Symbol) = false
      },
      ((1 to i) map { j => makeTypeVar("Arg" + j) }) :+ makeTypeVar("Ret") toSeq)
  }
  val typeDefs = Seq(Int, Boolean, Unit, CPtr) ++ Functions map makeDef

  def makeTypeVar(s: String): Symbol = {
    new Symbol {
      def name = s
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
      typeInfo = new TypeInfo {
        def members = Seq()
        def isDerived(s: Symbol) = false
      }
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

class Trait(var body: Tree, var composedTraits: Seq[Tree]) extends Tree {
  def children = Seq(body)

  override def toString = "trait < " + (composedTraits map {_.toString} mkString ",")  + body.toString
}

class Block(var children: Seq[Tree]) extends Tree {
  override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
}

class If(var condition: Tree, var ifTrue: Tree, var ifFalse: Tree) extends Tree {
  def children = Seq(condition, ifTrue, ifFalse)

  override def toString = "if(" + condition.toString + ") " + ifTrue.toString + (ifFalse match { case b: Block if b.children.isEmpty => "" case _ => " else " + ifFalse.toString })
}

class While(var condition: Tree, var body: Tree) extends Tree {
  def children = Seq(condition, body)
  override def toString = "while(" + condition.toString + ") " + body.toString
}

class Literal(var value: Any) extends Tree {
  def children = Seq()

  override def toString = "[lit:" + (if (value == null) "()" else value.toString) + "]"
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
        if (!exprOnly) ad.argName = transform(ad.argName)
        if (!exprOnly) ad.typeTree = transform(ad.typeTree)
      }
      case vd: ValDef => {
        if (!exprOnly) vd.valName = transform(vd.valName)
        if (!exprOnly) vd.typeTree = transform(vd.typeTree)
        vd.value = vd.value.map { v => transform(v) }
      }
      case td: TypeDef => {
        if (!exprOnly) td.typeName = transform(td.typeName)
        td.value = td.value.map(transform)
      }
      case dd: DefDef => {
        if (!exprOnly) dd.defName = transform(dd.defName)
        if (!exprOnly) dd.arguments = transformSeq(dd.arguments).asInstanceOf[Seq[ArgDef]]
        if (!exprOnly) dd.returnTypeTree = transform(dd.returnTypeTree)
        dd.body = dd.body map transform
      }
      case s: Struct => {
        if (!exprOnly) s.arguments = transformSeq(s.arguments).asInstanceOf[Seq[ArgDef]]
        if (!exprOnly) s.thisTree = transform(s.thisTree)
        s.composedTraits = transformSeq(s.composedTraits)
        s.content = transform(s.content)
      }
      case t: Trait => {
        if(!exprOnly) t.composedTraits = transformSeq(t.composedTraits)
        t.body = transform(t.body)
      }
      case b: Block => {
        b.children = transformSeq(b.children)
      }
      case s: Select => {
        s.from = transform(s.from)
        if (!exprOnly) s.memberName = transform(s.memberName)
      }
      case a: Assign => {
        a.dest = transform(a.dest)
        a.value = transform(a.value)
      }
      case i: If => {
        i.condition = transform(i.condition)
        i.ifTrue = transform(i.ifTrue)
        i.ifFalse = transform(i.ifFalse)
      }
      case w: While => {
        w.condition = transform(w.condition)
        w.body = transform(w.body)
      }
      case n: New => {
        if (!exprOnly) n.args = transformSeq(n.args)
        if (!exprOnly) n.typeTree = transform(n.typeTree)
      }
      case o: ObjectDef => {
        if (!exprOnly) o.objName = transform(o.objName)
        o.body = transform(o.body)
      }
      case ta: cast.TypeAttr => {
        ta.tree = transform(ta.tree)
      }
      case bc: cast.BitCast => {
        bc.ptr = transform(bc.ptr)
        if (!exprOnly) bc.typeTree = transform(bc.typeTree)
      }
      case uc: cast.UpCast => {
        uc.value = transform(uc.value)
        if (!exprOnly) uc.typeTree = transform(uc.typeTree)
      }
      case _: Literal => ()
      case _: Name => ()
      case _: Sym => ()
      //case x => x 
    }
    if (doTransform.isDefinedAt(tt)) doTransform(tt.asInstanceOf[Tree])
    else tt
  }

}

