package nasc

trait Phase[In, Out] extends AbstractPhase[In, Out] {
  def name: String
  def execute(input: In): Out
  def process(input: In): Out = {
    if(G.verbose) println("Phase " + name + " ==============")
    val res = execute(input)
    G.pp.beginPhase(name)
    res match {
      case cu : CompilationUnit => G.pp.prettyPrint(cu.root)
      case s : String => G.pp.print(s)
      case _ => ()
    }
    G.pp.endPhase()
    if(G.verbose) println(res)
    return res
  }
}

trait AbstractPhase[In, Out] {
  def process(input: In): Out
  def ++[Out2](second: AbstractPhase[Out, Out2]): AbstractPhase[In, Out2] = {
    val first = this
    new AbstractPhase[In, Out2] {
      def process(input: In) = {
        val res = first.process(input)
        second.process(res)
      }
    }
  }
}
