package nasc

class SymPhase extends Phase[CompilationUnit, CompilationUnit] {

  def name = "symbols"

  var contextHistory : Map[Tree, Context] = Map()
    
  case class Context(values: Map[String, List[Symbol]], next : Option[Context]) {
    def add(name: String, v: Symbol) = {
      Context(values + (name -> (v :: values.getOrElse(name, List()))), next)
    }
    def getFirstId(name: String) : Option[IdSymbol] =
      values.getOrElse(name, List()).collect { case s: IdSymbol => s } match { case List() => next.flatMap(_.getFirstId(name)) case id::ids => Some(id)}
    def getFirstType(name: String) = values(name).collect { case s: TypeSymbol => s }.first
    def contains(name: String) = values.contains(name) && (!values(name).isEmpty)
    def chain(o : Context) : Context = next match { case None => Context(values, Some(o)) case Some(c) => Context(values, Some(c.chain(o))) }
    override def toString = "{\n" + (values.map { case (k, v) => "\t" + k + " -> " + v + "\n"}).mkString + "next => " + next.getOrElse("{}").toString + "\n}"
  }

  def execute(cu: CompilationUnit): CompilationUnit = {
    val endCtx = processStmt(Context(Map(), None), cu.root)
    cu
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

  def processStmt(ctx: Context, e: Tree): Context = {
    // Link symbol to def
    e match {
      case id @ Id(name) => {
          id.symbol = ctx.getFirstId(name).getOrElse(Utils.error("Unknown id : " + name + " -- " + ctx))
      }
      case _ => ()
    }
    // Link type symbols
    e.subTypeExprs.foreach(linkTypeExpr(ctx, _))
    // Update context
    val newCtx = e match {
      case sd: SymDef => {
        sd.symbols.foldLeft(ctx) {
          case (ctx, sym) => {
            ctx.add(sym.name, sym)
          }
        }
      }
      case _ => {
        ctx
      }
    }
    val importedCtx = e match {
      case sd : StructDefinition => {
        sd.traits.foldLeft(newCtx) { case (ctx, t) =>
          ctx.chain(contextHistory(t.symbol.definition.asInstanceOf[TraitDefinition].body))
        }
      }
      case _ => newCtx
    }
    val finalCtx = e.children.foldLeft(importedCtx)(processStmt)
    contextHistory += e -> finalCtx
    if (e.scoped) newCtx
    else finalCtx
  }

}