package nasc

class ParsePhase extends Phase[String, ast.syntax.Tree] {
  def name = "parse"
  def execute(source: String) = new ast.syntax.Block(
   /*Defs.builtinTypeDef ::
      Defs.builtinFunDefs ++
      Defs.externFunDefs ++*/
      Grammar.parseAll(Grammar.program, source).get.children 
  )
}