package nasc

class InlineIRPhase extends Phase[Tree, Tree] {
  def name = "inline-ir"
  def execute(t: Tree): Tree = {
    t transform {
      case a: Apply if a.function.isInstanceOf[Name] && a.function.name == "__ir__" => {
        InlineIR.fromString(a.arguments.head match { case l: Literal => l.value.toString })
      }
    }
  }

}