package nasc

object Utils {
  def repsep(s: Seq[String]): String = repsep(s, ", ")
  def repsep(s: Seq[String], sep: String): String = {
    if (s.size == 0) ""
    else s.tail.foldLeft(s.head)(_ + sep + _)
  }
  def assert(c: Boolean) = if (!c) throw new RuntimeException("Assert failed")
  def error(s: String) = throw new RuntimeException(s)
  def symbolOr(s: Symbol, alt: String) = {
    if (s == null) {
      alt + "?"
    } else "[" + s.uniqueName + " : " + (if (s.typeSymbol == null) "?" else s.typeSymbol) + "]"
  }
}

object TreeUtils {
  def flattenBlocks(s: Seq[Tree]) = {
    s flatMap {
      case b: Block => b.children
      case x => Seq(x)
    }
  }
  def simplifyBlocks(tree: Tree) = // TODO users of this function are most likely trying to traverse global definitions 
    tree transform {               // do it better ! (symbol.scope, symbol.parent, ... ?)
      case b: Block => {
        b.children = flattenBlocks(b.children)
        b
      }
      case t => t
    }
}