package nasc

object Utils {
  def repsep(s: Iterable[String]): String = repsep(s, ", ")
  def repsep(s: Iterable[String], sep: String): String = {
    if (s.size == 0) return ""
    s.tail.foldLeft(s.head)(_ + sep + _)
  }
  def error(s: String) = throw new RuntimeException(s)
  def symbolOr(s : Symbol, alt : String) = if(s == null) alt else "[" + s.uniqueName + " : " + (if(s.ty == null) "?" else s.ty) + "]"
}