package nasc

trait Trees {

  type Tree_t <: Tree
  type Apply_t <: Apply with Tree
  type Id_t <: Id with Tree
  type ValDef_t <: ValDef with Tree
  type Block_t <: Block with Tree
  type Literal_t[T] <: Literal[T] with Tree

  trait Tree {
    def children: List[Tree]
  }

  trait Apply extends Tree {
    def function: Tree_t
    def arguments: List[Tree_t]
    def children = function :: arguments

    override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
  }

  trait Id extends Tree {
    def children = List()
  }

  trait ValDef extends Tree {
    def valName: Id_t
    def value: Option[Tree_t]
    def children = valName :: value.toList

    override def toString = "val " + valName + " : ?" + (value.map(" = " + _.toString).getOrElse(""))
  }

  trait Block extends Tree {
    override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
  }

  trait Literal[T] extends Tree {
    def value: T
    def children = List()
  }

}
trait GenericTrees extends Trees {
  type Block_t = Block
  class Block(val a: Int, val b: Int) extends Tree with super.Block {def children = List()}
  def Block(a : Int, b : Int) = new Block(a, b)
}

trait SyntaxTrees extends Trees {
  type Apply_t = Apply
  type Id_t = Id
  type ValDef_t = ValDef
  type Literal_t[T] = Literal[T]

    case class Literal[T](value: T) extends super.Literal[T] with Tree

    case class Apply(override val function: Tree, override val arguments: List[Tree]) extends Apply with Tree

    case class Id(name: String) extends super.Id with Tree {
      override def toString = name + "?"
    }

    case class ValDef(valName: Id, typeExpr: TypeExpr, value: Option[Tree]) extends super.ValDef with Tree
  

}
/*
trait LinkedTrees extends Trees {
  type Tree = t.Tree
  type Apply = t.Apply
  type Id = t.Id
  type ValDef = t.ValDef
  type Block = t.Block
  type Literal[T] = t.Literal[T]
  object t {

    trait Tree extends BaseTree {
      def referencedSymbol: Option[Symbol]
      def hasSymbol = !referencedSymbol.isEmpty
    }

    case class Literal[T](value: T) extends BaseLiteral[T] with Tree {
      def referencedSymbol = None
    }

    case class Apply(function: Tree, arguments: List[Tree]) extends BaseApply with Tree {
      def referencedSymbol = None
    }

    case class Id(val symbol: IdSymbol) extends BaseId with Tree {
      def children = List()
      def referencedSymbol = Some(symbol)

      override def toString = symbol.toString
    }

    case class ValDef(valName: Id, typeExpr: TypeExpr, value: Option[Tree]) extends BaseValDef with Tree {
      def referencedSymbol = None
    }

    case class Block(children: List[Tree]) extends BaseBlock with Tree {
      def referencedSymbol = None
    }

  }

}*/

package object ast {
  //object generic extends GenericTrees
  object syntax extends SyntaxTrees with GenericTrees
  //object linked extends LinkedTrees with GenericTrees
}

object s {
  import ast._
  def aaa() = {
	  val a = syntax.Block(1,2)
  }
}

