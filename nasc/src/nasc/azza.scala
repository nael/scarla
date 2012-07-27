package nasc

class TestT extends FF with SyntaxTrees {

  def doTransform(t: Tree): Tree = {
    println("++++++++++++++ " + t)
    println()
    t
  }
}
trait FF { self: Trees =>
  def doTransform(t: Tree): Tree
}

trait Trees {

  type Tree <: BaseTree
  type NameTree <: Tree

  trait BaseTree {
    def children: List[Tree]
  }

  trait TypeTree extends BaseTree

  class Apply(val function: Tree, val arguments: List[Tree]) extends BaseTree {
    def children = function :: arguments

    override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
  }

  class ValDef(val valName: NameTree, val typeTree: TypeExpr, val value: Option[Tree]) extends BaseTree {
    def children = valName :: value.toList

    override def toString = "val " + valName + " : ?" + (value.map(" = " + _.toString).getOrElse(""))
  }

  class Block(val children: List[Tree]) extends BaseTree {
    override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
  }

  class Literal[T](val value: T) extends BaseTree {
    def children = List()

    override def toString = "[lit:" + value.toString + "]"
  }

}

object abs {
    def transformList(inTrees : Trees, outTrees: Trees)(f: inTrees.Tree => outTrees.Tree, t: List[inTrees.Tree]): List[outTrees.Tree] = { t.map(transform(inTrees,outTrees)(f, _)) }
    def transform(inTrees : Trees, outTrees: Trees)(f: inTrees.Tree => outTrees.Tree, t: inTrees.Tree): outTrees.Tree = {
      def tr(t : inTrees.Tree):outTrees.Tree = transform(inTrees, outTrees)(f, t)
      def trl(t : List[inTrees.Tree]):List[outTrees.Tree] = transformList(inTrees,outTrees)(f, t)
      val ttt = (t match {
        case a: inTrees.Apply => {
          new outTrees.Apply(tr(a.function), trl(a.arguments))
        }
        case vd: inTrees.ValDef => {
          new outTrees.ValDef(tr(vd.valName).asInstanceOf[outTrees.NameTree], vd.typeTree, vd.value.map { x => tr(x)})
        }
        case b: inTrees.Block => {
          new outTrees.Block(trl(b.children))
        }
        case lit: inTrees.Literal[_] => {
          lit
        }
        case name: inTrees.NameTree => {
          name
        }
        //case x => x 
      }).asInstanceOf[outTrees.Tree]
      f(ttt)
    }

}

trait GenericTrees extends Trees {
  object transform {
    trait Transformer { def apply(t: Tree): Tree }
    
  
  }

}

trait SyntaxTrees extends Trees {
  type Tree = BaseTree
  type NameTree = Name
  class Name(val name: String) extends BaseTree {
    def children = List()
    override def toString = name + "?"
  }

}

trait LinkedTrees extends Trees {
  type Tree <: BaseTree
  type NameTree = Id with Tree
  class Id(val symbol: Symbol) extends BaseTree {
    def children = List()
  }
}

trait TypedTrees extends Trees {
  type Tree = BaseTree { def typeSymbol: TypeSymbol }
}

package object ast {
  object syntax extends SyntaxTrees with GenericTrees
  object linked extends LinkedTrees with GenericTrees
  object typed extends TypedTrees with LinkedTrees with GenericTrees
}

object s {
  import ast._
  def aaa() = {
    val a: typed.Tree = new typed.Block(List()) { def typeSymbol = null }
  }
}

