package nasc

class ParsePhase extends Phase[String, Tree] {
  def name = "parse"
  def execute(source: String) =
    try {
    new Block(
      Builtin.typeDefs ++ Grammar.parseAll(Grammar.program, source).get.children
    )
    } catch { case _ => { println("Parsing error :"); println(source); Utils.error("")} }
}