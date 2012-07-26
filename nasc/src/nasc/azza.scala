package nasc

trait Trees {
  type Tree <: BaseTree
  type Apply <: BaseApply with Tree
  type Id <: BaseId with Tree
  type ValDef <: BaseValDef with Tree
  type Block <: BaseBlock with Tree
  type Literal[T] <: BaseLiteral[T] with Tree

  trait BaseTree {
    def children: List[Tree]
  }

  trait BaseApply extends BaseTree {
    def function: Tree
    def arguments: List[Tree]
    def children = function :: arguments
    
    override def toString = function.toString + "(" + Utils.repsep(arguments.map(_.toString)) + ")"
  }

  trait BaseId extends BaseTree {
    def children = List()
  }

  trait BaseValDef extends BaseTree {
    def valName : Id
    def value : Option[Tree]
    def children = valName :: value.toList
    
    override def toString = "val " + valName + " : ?" + (value.map(" = " + _.toString).getOrElse(""))
  }

  trait BaseBlock  extends BaseTree {
    override def toString = "{\n" + children.map(_.toString).mkString("\n") + "\n}"
  }
  
  trait BaseLiteral[T]  extends BaseTree {
    def value: T
    def children = List()
  }

}
trait GenericTrees extends Trees {
  type Block = t.Block
  object t {
    class Block(val a :Int, val b : Int) extends BaseTree {}
  }
  
}

trait SyntaxTrees extends Trees  {
  type Apply = t.Apply
  type Id = t.Id
  type ValDef = t.ValDef
  type Block = t.Block
  type Literal[T] = t.Literal[T]
  object t {
    
    trait SymbolDef {
      def definedSymbols : List[Symbol]
    }
    
    case class Literal[T](value: T) extends BaseLiteral[T] with BaseTree

    case class Block(children: List[Tree]) extends BaseBlock with BaseTree 

    case class Apply(function: Tree, arguments: List[Tree]) extends BaseApply with BaseTree

    case class Id(name: String) extends BaseId with BaseTree {
      override def toString = name + "?"
    }

    case class ValDef(valName: Id, typeExpr: TypeExpr, value: Option[Tree]) extends BaseValDef with BaseTree
  }

}

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

}


package object ast {
  //object generic extends GenericTrees
   object syntax extends SyntaxTrees with GenericTrees
  object linked extends LinkedTrees with GenericTrees
}

object s {
  import ast._
  def aaa() = {

  }
}

