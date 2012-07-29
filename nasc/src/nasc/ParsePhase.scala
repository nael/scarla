package nasc

class ParsePhase extends Phase[String, Tree] {
  def name = "parse"
  def execute(source: String) =
    new Block(
      Builtin.typeDefs ++ Grammar.parseAll(Grammar.program, source).get.children
    )
}