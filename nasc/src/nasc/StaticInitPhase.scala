package nasc

class StaticInitPhase extends Phase[Tree, Tree] {

  def name = "static-init"

  def execute(tree: Tree): Tree = {
    val t = TreeUtils.simplifyBlocks(tree)
    val init = t.children flatMap {
      case vd: ValDef => {
        val v = vd.value
        vd.value = None
        v map { vv => new Assign(new Sym(vd.valName.symbol), vv)}
      }
      case _ => Seq()
    } toSeq
    val initSym = new Symbol {
      val name = "__init"
      var typeSymbol: Symbol = null
      var isType: Boolean = false
      var definition: Def = null
    }
    val initDef = new DefDef(new Sym(initSym), Seq(), new Sym(Builtin.Unit.symbol), Some(new Block(init)))
    initDef.attr += attributes.Native("__init")
    initSym.definition = initDef
    Typer.typeTree(new Block(t.children.toSeq :+ initDef))
  }
}