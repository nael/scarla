package nasc

object Utils {
  def repsep(s: Seq[String]): String = repsep(s, ", ")
  def repsep(s: Seq[String], sep: String): String = {
    if (s.size == 0) return ""
    s.tail.foldLeft(s.head)(_ + sep + _)
  }
  def assert(c: Boolean) = if (!c) throw new RuntimeException("Assert failed")
  def error(s: String) = throw new RuntimeException(s)
  def symbolOr(s: Symbol, alt: String) = {
    if (s == null) {
      alt + "?"
    } else "[" + s.uniqueName + " : " + (if (s.typeSymbol == null) "?" else s.typeSymbol) + "]"
  }
}