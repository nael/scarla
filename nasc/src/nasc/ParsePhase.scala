package nasc

class ParsePhase extends Phase[String, CompilationUnit] {
  def name = "parse"
  def execute(source: String) = new CompilationUnit(Block(
    Defs.builtinTypeDef ::
      Defs.builtinFunDefs ++
      Defs.externFunDefs ++
      Grammar.parseAll(Grammar.program, source).get.children))
}