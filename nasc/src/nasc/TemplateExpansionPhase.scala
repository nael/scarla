package nasc

class TemplateExpansionPhase extends Phase[Tree, Tree] {
  def name = "template-expansion"

  var done = Set[Tuple2[Symbol, Seq[Symbol]]]()
  def execute(t: Tree): Tree = {

    def isConcrete(t: Tree): Boolean = t match {
      case s: Sym => s.symbol.definition.isInstanceOf[TypeDef]
      case a: Apply => isConcrete(a.function) && a.arguments.forall(isConcrete)
      case _ => false
    }
    var result = t
    var trees = Seq[Tree]()
    var modified = true
    while (modified) {
      modified = false
      trees = Seq()
      // collect a mapping : type -> seq((typedef, argument tuple at usage sites))
      val usages = result collect {
        case a: Apply if a.function.hasSymbol && a.function.symbol.isType && a.arguments.forall(isConcrete) => {
          (a.function.symbol, a.function.symbol.definition.asInstanceOf[TypeDef], a.arguments)
        }
      } groupBy { case (a, b, c) => (a, b) } mapValues { _ map { _._3 map { _.symbol } } toSet }

      usages foreach {
        case ((ty, d), usages) =>

          usages foreach { args =>
            if (!done.contains((ty, args))) {
              val dd = d.duplicate.asInstanceOf[TypeDef]

              val tv = dd.typeVars map { _.varName.symbol }
              val typeEnv = tv zip args toMap

              dd.typeName = new Sym(Typer.typeTreeSymbol(new Apply(new Sym(d.typeName.symbol), args map { new Sym(_) })))
              dd.typeVars = Seq()

              //TODO erk : force thisTree re-creation
              //dd.value = dd.value  match { case Some(s: Struct) => Some(new Struct(s.arguments, s.composedTraits, s.content)) case x => x }
              done += ((ty, args))
              modified = true
              trees :+= dd transform {
                case s: Sym if typeEnv contains s.symbol => {
                  new Sym(typeEnv(s.symbol))
                }
              }
            }
          }
      }
      result = TreeUtils.simplifyBlocks(new Block(trees :+ result))
    }
    result
  }
}