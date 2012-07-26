package nasc

case class Context(values: Map[String, List[Symbol]], next: Option[Context]) {
  def add(name: String, v: Symbol) = {
    Context(values + (name -> (v :: values.getOrElse(name, List()))), next)
  }
  def getFirstId(name: String): Option[IdSymbol] =
    values.getOrElse(name, List()).collect { case s: IdSymbol => s } match { case List() => next.flatMap(_.getFirstId(name)) case id :: ids => Some(id) }
  def getFirstType(name: String) = values(name).collect { case s: TypeSymbol => s }.first
  def contains(name: String) = values.contains(name) && (!values(name).isEmpty)
  def chain(o: Context): Context = next match { case None => Context(values, Some(o)) case Some(c) => Context(values, Some(c.chain(o))) }
  override def toString = "{\n" + (values.map { case (k, v) => "\t" + k + " -> " + v + "\n" }).mkString + "next => " + next.getOrElse("{}").toString + "\n}"
}

class SymPhase extends Phase[ast.syntax.Tree, ast.linked.Tree] {

  def name = "symbols"

  var contextHistory: Map[ast.syntax.Tree, Context] = Map()

  def execute(tree: ast.syntax.Tree): ast.linked.Tree = {
    val endCtx = process(Context(Map(), None), tree)
    endCtx.tree
  }
  
  case class ContextTree(ctx : Context, tree : ast.linked.Tree)
  case class ContextTrees(ctx : Context, trees : List[ast.linked.Tree] = List())
  def processList(ctx : Context, es : List[ast.syntax.Tree]) : ContextTrees = {
      es.foldLeft(ContextTrees(ctx)) { case (ContextTrees(ctx, ls), e) =>
        val ctxTree = process(ctx, e)
        ContextTrees(ctxTree.ctx, ls :+ ctxTree.tree)
      }
  }
  
  def link(ctx: Context, t : ast.syntax.Tree) : ContextTree = {
    //e.subTypeExprs.foreach(linkTypeExpr(ctx, _))
    
    t match {
      case ast.syntax.t.Block(ls) => {
        val finalCtx = processList(ctx, ls)
        ContextTree(finalCtx.ctx, ast.linked.t.Block(finalCtx.trees))
      }
      
      case ast.syntax.t.Apply(function, args) => {
        val f = process(ctx, function)
        val a = processList(f.ctx, args)
        ContextTree(a.ctx, ast.linked.t.Apply(f.tree, a.trees))
      }
      
      case ast.syntax.t.Id(name) => {
        ctx.getFirstId(name) match {
          case Some(sym) => ContextTree(ctx, ast.linked.t.Id(sym))
          case None => Utils.error("Unknown identifier " + name)
        }
      }
      
      case ast.syntax.t.ValDef(valName, valType, value) => {
        val sym = new IdSymbol(valName.name, null)
        val ctx1 = ctx.add(sym.name, sym)
        
        val ContextTree(ctx2, vn) = process(ctx1, valName)
        val v = value.map(u => process(ctx2, u).tree)
        val vd = ast.linked.t.ValDef(vn.asInstanceOf[ast.linked.t.Id], valType, v)
        ContextTree(ctx2, vd)
      }
      
      case a @ ast.syntax.t.Literal(u) => ContextTree(ctx, ast.linked.t.Literal(u))
      
    }
    
  }
  
  def process(ctx: Context, e: ast.syntax.Tree): ContextTree = {
    /*val importedCtx = e match {
      case sd: StructDefinition => {
        sd.traits.foldLeft(newCtx) {
          case (ctx, t) =>
            ctx.chain(contextHistory(t.symbol.definition.asInstanceOf[TraitDefinition].body))
        }
      }
      case _ => newCtx
    }*/
    val finalCtxTree = link(ctx, e)
    contextHistory += e -> finalCtxTree.ctx
    if (e.scoped) ContextTree(ctx, finalCtxTree.tree)
    else finalCtxTree
  }
  
  def linkTypeExpr(ctx: Context, e: TypeExpr): Unit = {
    try {
      e match {
        case tId @ TypeId(u) =>
          tId.symbol = ctx.getFirstType(u)

        case tApl @ TypeApply(name, args) => {
          args.foreach(linkTypeExpr(ctx, _))
          tApl.symbol = ctx.getFirstType(name)
        }
      }
    } catch { case x => throw new RuntimeException("Unknown type : " + e + "(" + x.getMessage() + ")") }
  }
}