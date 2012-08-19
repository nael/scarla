package nasc

case class Context(values: Map[String, Seq[Symbol]], next: Option[Context]) {
  def add(name: String, v: Symbol) = {
    Context(values + (name -> (v +: values.getOrElse(name, Seq()))), next)
  }

  def getFirst(name: String, isType: Boolean): Option[Symbol] =
    values.getOrElse(name, Seq()) filter { _.isType == isType } match { case Seq() => next.flatMap(_.getFirst(name, isType)) case id :: ids => Some(id) }
  def getAll(name: String, isType: Boolean): Seq[Symbol] =
    (values.getOrElse(name, Seq()) filter { _.isType == isType }) ++ (next match { case None => Seq() case Some(c) => c.getAll(name, isType) })
  def contains(name: String) = values.contains(name) && (!values(name).isEmpty)
  def chain(o: Context): Context = next match { case None => Context(values, Some(o)) case Some(c) => Context(values, Some(c.chain(o))) }
  override def toString = "{\n" + (values.map { case (k, v) => "\t" + k + " -> " + v + "\n" }).mkString + "next => " + next.getOrElse("{}").toString + "\n}"
}

class SymPhase extends Phase[Tree, Tree] {

  def name = "symbols"

  var contextHistory: Map[Tree, Context] = Map()

  def execute(tree: Tree): Tree = {
    new Linker().transform(tree)
  }

  class Linker extends TreeTransform {
    var ctx = Context(Map(), None)

    val doTransform: PartialFunction[Tree, Tree] = {
      case name: Name => {
        val syms = ctx.getAll(name.name, name.isTypeName)
        if (syms.isEmpty) name
        else new Sym(syms)
      }
    }
    override def transform(t: Tree): Tree = {
      val oldContext = ctx

      def name(x: Tree) = x match {
        case name: Name => Some(name.name)
        case _ => None
      }

      def add(nameTree: Tree, sym: String => Symbol): Context = {
        nameTree match {
          case n: Name => ctx.add(n.name, sym(n.name))
          case s: Sym => ctx.add(s.symbol.name, s.symbol)
          case _ => ctx
        }
      }

      ctx = t match {
        case vd: ValDef => {
          add(vd.valName, valName => new Symbol {
            def name = valName
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = vd
          })
        }
        case td: TypeDef => {
          add(td.typeName, typeName => new Symbol {
            def name = typeName
            var typeSymbol: Symbol = null
            var isType = true
            var definition: Def = td
          })
        }
        case ad: ArgDef => {
          add(ad.argName, argName => new Symbol {
            def name = argName
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = ad
          })
        }
        case dd: DefDef => {
          add(dd.defName, defName => new Symbol {
            def name = defName
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = dd
          })
        }
        case od: ObjectDef => {
          add(od.objName, objName => new Symbol {
            def name = objName
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = od
          })
        }
        case st: Struct => {
          add(st.thisTree, _ => st.thisTree.symbol)
        }
        case _ => ctx
      }
      val res = super.transform(t)
      t match {
        case _: Block => ctx = oldContext
        case _ => ()
      }
      res
    }
  }
  /*  
  case class ContextTree(ctx : Context, tree : ast.linked.Tree)
  case class ContextTrees(ctx : Context, trees : Seq[ast.linked.Tree] = Seq())
  def processSeq(ctx : Context, es : Seq[ast.syntax.Tree]) : ContextTrees = {
      es.foldLeft(ContextTrees(ctx)) { case (ContextTrees(ctx, ls), e) =>
        val ctxTree = process(ctx, e)
        ContextTrees(ctxTree.ctx, ls :+ ctxTree.tree)
      }
  }
  
  def link(ctx: Context, t : ast.syntax.Tree) : ContextTree = {
    //e.subTypeExprs.foreach(linkTypeExpr(ctx, _))
    
    t match {
      case ast.syntax.t.Block(ls) => {
        val finalCtx = processSeq(ctx, ls)
        ContextTree(finalCtx.ctx, ast.linked.t.Block(finalCtx.trees))
      }
      
      case ast.syntax.t.Apply(function, args) => {
        val f = process(ctx, function)
        val a = processSeq(f.ctx, args)
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
  }*/
}