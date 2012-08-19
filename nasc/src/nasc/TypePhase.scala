package nasc

trait TypeInfo {
  def members: Seq[Symbol]
  def derived: Seq[Symbol] = Seq()

  def vals = members filter { m => m.definition != null && (m.definition.isInstanceOf[ValDef]) }
}

case class Conversion(from: Symbol, to: Symbol, conv: Tree => Tree)

object Glob {
  var conversions = Seq[Conversion]()
}

class TypePhase extends Phase[Tree, Tree] {

  def name = "type"
  def execute(t: Tree): Tree = {
    val typedTree = Typer.typeTree(t)
    Typer.check(typedTree)
    typedTree
  }
}

object Typer {
  val NoType = new Symbol {
    def name = "<undefined>"
    var typeSymbol: Symbol = null
    var isType = true
    var definition: Def = null

    typeInfo = new TypeInfo {
      def members = Seq()
    }
  }

  def typeTree(t: Tree): Tree = {
    val tt = t transform { case d: TypeDef => makeThis(d) } transform { case s: Struct => expandValCtor(s) }
    val typeDefs = tt collect { case d: TypeDef => d }
    populateTypeInfos(typeDefs)

    val typer = new TypeTransform()
    val typedTree = typer.transform(tt)
    typedTree
  }

  def expandValCtor(s: Struct): Struct = {
    var vds = Seq[Tree]()
    s.arguments filter { _.hasAttr[attributes.Val] } foreach { arg =>
      arg.attr -= attributes.Val()
      val argSym = new Symbol {
        val name = arg.argName.name
        var typeSymbol: Symbol = null
        var isType = false
        var definition: Def = arg
      }
      val vd = new ValDef(arg.argName, arg.typeTree, Some(new Sym(argSym)))
      arg.argName.symbol.definition = vd
      vds :+= vd
      arg.argName = new Sym(argSym)
    }
    s.content.asInstanceOf[Block].children ++= vds //TODO berk cast
    s
  }

  // Takes a type tree and return all possible overloaded type symbols
  def typeTreeSymbols(t: Tree): Seq[Symbol] = {
    t match {

      case s: Sym => s.symbols filter { _.isType }

      case a: Apply => {
        val fSyms = typeTreeSymbols(a.function)
        val typeValsO = a.arguments map typeTreeSymbols
        Utils.assert(typeValsO forall { _.size == 1 })
        val typeVals = typeValsO map { _.first }
        var syms = Seq[Symbol]()
        fSyms.foreach { s =>
          s.definition match {
            case td: TypeDef => {
              if (td.typeVars.size == typeVals.size) {
                val typeEnv = td.typeVars map { _.symbol } zip typeVals

                syms +:= s.derivedSymbols find { _.typeVars == typeEnv } getOrElse {
                  val newSym = new Symbol {
                    def name = {
                      val extractedLocalValue = Utils.repsep(typeVals map { _.toString })
                      s + "[" + extractedLocalValue + ("]")
                    }
                    var typeSymbol: Symbol = null
                    var isType = true
                    var definition: Def = s.definition
                    typeVars = typeEnv
                    typeInfo = s.typeInfo
                  }

                  s.derivedSymbols +:= newSym
                  newSym
                }
              }
            }
            case _ => ()
          }
        }
        syms
      }
    }
  }

  def typeTreeSymbol(t: Tree): Symbol = {
    val syms = typeTreeSymbols(t)
    if (syms.size != 1) Utils.error("Wrong od for " + t + " : " + syms)
    t.typeSymbol = NoType
    syms.head
  }

  def convert(tree: Tree, to: Symbol): Option[Tree] = {
    Utils.assert(tree.typed && to.isType)
    //    println("Trying conversion " + to + " -> " + tree.typeSymbol + " --- " + tree)
    if (tree.typeSymbol == to) return Some(tree)
    else if (to == Builtin.Unit.symbol) {
      Some(Typer.typeTree(new Block(Seq(tree, new Literal(null)))))
    } else {
      Glob.conversions find { x => x.from == tree.typeSymbol && x.to == to } map { conv =>
        Typer.typeTree(conv.conv(tree))
      }
    }
  }

  class TypeTransform extends TreeTransform {
    val doTransform: PartialFunction[Tree, Tree] = {
      case s: Sym if s.symbol.typed => {
        s.typeSymbol = s.symbol.typeSymbol
        s
      }
      case s: Sym if !s.typed => {
        s.typeSymbol = s.symbol.definition match {
          case vd: ValDef => {
            typeTreeSymbol(vd.typeTree)
          }
          case ad: ArgDef => {
            typeTreeSymbol(ad.typeTree)
          }
          case dd: DefDef => {
            val ret = typeTreeSymbol(dd.returnTypeTree)
            val a = dd.arguments map { arg => typeTreeSymbol(arg.typeTree) }
            typeTreeSymbol(new Apply(new Sym(Builtin.Functions(a.size).symbol), a.map { new Sym(_) } :+ new Sym(ret)))
          }
          case td: TypeDef => {
            NoType
          }
          case _ => null
        }

        if (s.typed) // TODO Seems magic but maybe useful ?
          s.symbol.typeSymbol = s.typeSymbol
        s
      }

      case sel: Select if !sel.typed => {
        if (sel.from.typed) {
          val ty = sel.from.typeSymbol.typeInfo
          ty.members find { _.name == sel.memberName.name } match {
            case Some(member) => {
              sel.memberName = transform(new Sym(member))
              sel.typeSymbol = sel.memberName.typeSymbol
              sel
            }
            case None => sel
          } // TODO overloads ...
        } else { println("Couldnt type " + sel + " because " + sel.from + " is not typed"); sel }
      }

      case a: Assign if !a.typed => {
        if (a.dest.typed && a.value.typed) {
          convert(a.value, a.dest.typeSymbol) match {
            case Some(v) => {
              a.value = v
              a.typeSymbol = Builtin.Unit.symbol
            }
            case _ => ()
          }
          a
        } else a
      }

      case b: Block if !b.typed => {
        b.typeSymbol =
          if (b.children.isEmpty) Builtin.Unit.symbol
          else if (b.children.last.typed) b.children.last.typeSymbol
          else null
        b
      }

      case d: Def if !d.typed => {
        d match {
          case vd: ValDef if !vd.value.isEmpty => {
            val v = vd.value.get
            if (v.typed) {
              convert(v, typeTreeSymbol(vd.typeTree)) match {
                case None => ()
                case Some(convertedV) => {
                  vd.value = Some(convertedV)
                  vd.typeSymbol = Builtin.Unit.symbol
                }
              }
            }
          }
          case _ => {
            d.typeSymbol = Builtin.Unit.symbol
          }
        }
        d
      }

      case s: Struct if !s.typed => {
        s.typeSymbol = NoType; s
      }

      case t: Trait if !t.typed => {
        t.typeSymbol = NoType; t
      }

      case ta: cast.TypeAttr if !ta.typed => {
        if (ta.tree.typed) {
          val ty = ta.tree.typeSymbol
          val reqAttr = (ty.definition.attr -- ta.remove) ++ ta.add
          ty.derivedSymbols find { sym =>
            sym.typeVars == ty.typeVars && sym.definition.attr == reqAttr
          } match {
            case Some(s) => ta.typeSymbol = s
            case None => Utils.error("Removing qualifier on type never seen before, should create")
          }
          ta
        } else ta
      }

      case bc: cast.BitCast if !bc.typed => {
        if (bc.ptr.typed) {
          bc.typeSymbol = typeTreeSymbol(bc.typeTree)
          bc
        } else bc
      }

      case uc: cast.UpCast if !uc.typed => {
        if (uc.value.typed) {
          //TODO check if uc.value.typeSymbol implements uc.typeTree
          uc.typeSymbol = typeTreeSymbol(uc.typeTree)
          uc
        } else uc
      }

      case i: If if !i.typed => {
        if (i.condition.typed && i.ifTrue.typed && i.ifFalse.typed) {
          (convert(i.condition, Builtin.Boolean.symbol), convert(i.ifTrue, i.ifFalse.typeSymbol)) match { // TODO haveCommonConv
            case (Some(cond), Some(tb)) => {
              i.condition = cond
              i.ifTrue = tb
              i.typeSymbol = i.ifFalse.typeSymbol
            }
            case _ => ()
          }
          i
        } else i
      }

      case w: While if !w.typed => {
        if (w.condition.typed && w.body.typed) {
          convert(w.condition, Builtin.Boolean.symbol) match {
            case Some(cond) => {
              w.condition = cond
              w.typeSymbol = Builtin.Unit.symbol
              w
            }
            case _ => w
          }
        } else w
      }

      case l: Literal if !l.typed => {
        l.typeSymbol = l.value match {
          case i: Int => Builtin.Int.symbol
          case b: Boolean => Builtin.Boolean.symbol
          case null => Builtin.Unit.symbol // TODO ugh
          case _ => Utils.error("Unknown literal type : " + l.value.getClass)
        }
        l
      }

      case n: New if !n.typed => {
        if (n.typeTree.typed) {
          //TODO check ctor args correctness
          n.typeSymbol = typeTreeSymbol(n.typeTree)
          n
        } else n
      }

      case a: Apply if !a.typed => {
        if (a.function.typed && a.arguments.forall(_.typed)) {
          if (Builtin.isFunction(a.function.typeSymbol)) {
            val fArgTypes = Builtin.functionArgTypes(a.function.typeSymbol)
            if (fArgTypes.size != a.arguments.size) Utils.error("Wrong argument count for " + a.function + " got " + a.arguments.size + " expected " + fArgTypes.size)
            val args = (fArgTypes zip a.arguments) map { case (ty, arg) => convert(arg, ty) }
            if (args.contains(None)) Utils.error("Wrong arg type : " + fArgTypes + " / " + a.arguments)

            a.arguments = args map { _.get }
            a.typeSymbol = Builtin.functionReturnType(a.function.typeSymbol)

            a
          } else Utils.error("Not callable : " + a.function.typeSymbol)
        } else a
      }
      case t => t
    }
  }

  // TODO here linearization of types & ... for now it just proceed in order and can fail (horribly)
  // also verify concrete types do not lack any unimplemented members
  def populateTypeInfos(tds: Seq[TypeDef]) = {
    tds.reverse foreach { td =>
      val ts = typeTreeSymbol(td.typeName)
      td.value foreach {
        case st: Struct => {
          val m = st.content.children collect {
            case vd: ValDef => vd.valName.symbol
            case dd: DefDef => dd.defName.symbol
          } toSeq
          val composed = st.composedTraits flatMap { _.symbol.typeInfo.members } // Worst way possible
          ts.typeInfo = new TypeInfo {
            override val derived = composed filter { s => !(m map { _.name } contains s.name) } // again
            def members = m
          }

          st.composedTraits foreach { tr =>
            Glob.conversions +:= Conversion(ts, tr.symbol, { t =>
              new cast.UpCast(t, new Sym(tr.symbol))
            })
          }
        }
        case t: Trait => {
          val m = t.body.children collect {
            case vd: ValDef => vd.valName.symbol
            case dd: DefDef => dd.defName.symbol
          }
          ts.typeInfo = new TypeInfo {
            def members = m.toSeq
          }
        }
        case s: Sym => {
          Utils.assert(s.symbol.typeInfo != null)
          ts.typeInfo = s.symbol.typeInfo
          s.symbol.derivedSymbols +:= ts
          ts.derivedSymbols +:= s.symbol
        }
        case x => println("No typeinfo for " + x)
      }
      td.typeVars.foreach { tv => tv.typeSymbol = NoType; tv.symbol.typeSymbol = NoType }
      ts.typeSymbol = NoType
    }
  }

  def makeThis(td: TypeDef): Tree = {
    td.value match {
      case Some(st: Struct) => {
        val ts = typeTreeSymbol(td.typeName)
        if (td.hasAttr[attributes.Move] || td.hasAttr[attributes.CopyThis]) {
          st.thisTree.symbol.typeSymbol = ts
          td
        } else {
          ts.derivedSymbols find { sym =>
            sym.typeVars == ts.typeVars && sym.definition.hasAttr[attributes.Move]
          } match {
            case None => {
              val ptrSym = new Symbol { // TODO way too ugly, time for type Ptr[T] = move T ?
                def name = td.typeName.name + "Ptr"
                var typeSymbol: Symbol = null
                var isType = true
                var definition: Def = null
              }
              val ptrTd = new TypeDef(new Sym(ptrSym), Seq(), Some(new Sym(ts)))
              ptrSym.definition = ptrTd
              ptrTd.attr += attributes.Move()

              Glob.conversions +:= Conversion(ts, ptrSym, { t: Tree =>
                new cast.TypeAttr(Set(attributes.Move()), Set(), t)
              })
              st.thisTree.symbol.typeSymbol = ptrSym
              new Block(Seq(ptrTd, td))
            }
            case Some(sym) => {
              st.thisTree.symbol.typeSymbol = sym
              td
            }
          }

        }
      }
      case _ => td
    }
  }

  def check(t: Tree): Unit = {
    var ok = true
    val untyped = t filter { !_.typed } toSet

    if (!untyped.isEmpty) {
      println("Untyped trees : " + untyped.map(_.toString))
      ok = false
    } else {
      if (G.verbose) println("Whole tree is typed !")
    }

    val syms = (t collect { case x if x.hasSymbol => x.symbol }) ++ (t collect { case x if x.typed => x.typeSymbol })
    syms.toSet.foreach { sym: Symbol =>
      if (sym.isType) {
        if (sym.typeInfo == null) {
          println("No type info on " + sym)
          ok = false
        }
      } else {
        if (!sym.typed) {
          println("Untyped symbol : " + sym + " : " + sym.typeSymbol)
          ok = false
        }
      }
    }

    val failed = t collect { case s: Sym if s.typed && s.typeSymbol != s.symbol.typeSymbol => s }
    if (!failed.isEmpty) {
      println("Bad symbols : ")
      failed.toSet foreach { s: Sym =>
        println(s.symbol + " : " + s.typeSymbol + " != " + s.symbol.typeSymbol)
      }
      //ok = false//TODO XXXXXX
    }

    if (!ok) {
      println("++++++++++++++++++++++++++++++++++++++++++++++")
      println(t)
      Utils.error("Typing failed")
    }
  }

}

/*object Typer {
  def typeTree(s: Tree) = {
    doTypeTree(s, true)
  }

  def typed(s: Seq[Tree]): Seq[Tree] = {
    s.map { st => typed(st) }
  }

  def typed[T <: Tree](s: T): T = {
    typeTree(s)
    s
  }

  def collectAggregateFields(d: AggregateDefinition[_]): Seq[Types.Aggregate.Element] = {
    val defBody = d.body.duplicate()
    defBody.children.map(doTypeTree(_, false)) // Ensure we know the fields types
    defBody.children.toSeq.reverse.foldLeft(Seq[Types.Aggregate.Element]()) {
      case (fields, vd: ValDefinition) => {
        Types.Aggregate.Field(vd.name, vd.valSymbol) :: fields
      }
      case (fields, fd: FunctionDefinition) => {
        Types.Aggregate.Function(fd.name, fd.funSymbol) :: fields
      }
      case (fields, a) => fields //throw new RuntimeException("Unexpected struct field " + a)
    }
  }

  def doTypeTree(tree: Tree, deep: Boolean): Unit = {
    if (tree.typed) return
    /*stmt match { // Type def's (and struct, ...) arguments
      case d: Definition => {
        d.arguments.foreach { arg =>
          
        }
      }
      case _ => ()
    }*/
    tree match {
      case d : Definition => { d.arguments.foreach(typeExpr) } // Make sur we typ'd arguments before typing the def
      case _ => (
    }
    tree match { // Type a symbol definition
      case fd: FunctionDefinition => {
        fd.returnType = resolveTypeExpr(fd.retTypeExpr)
        fd.funSymbol.ty = fd.functionType
      }
      case d: BuiltinFunDef => {
        d.funSymbol.ty = d.fun.functionType
      }
      case d: ExternFunDef => {
        d.funSymbol.ty = d.functionType
      }

      case d: TraitDefinition => {
        val tFields = collectAggregateFields(d)
        d.typeSymbol.definedType = new Types.Trait(d.typeSymbol, tFields)
      }

      case d: StructDefinition => {
        val sFields = collectAggregateFields(d)
        val mods = if (d.isValue) Types.ClassMods.ValueSet(Types.ClassMods.Val) else Types.ClassMods.ValueSet()
        d.typeSymbol.definedType = new Types.Struct(d.typeSymbol, d.traits.map(resolveTypeExpr(_).concreteType.asInstanceOf[Types.Trait]), sFields, mods)
        val argTypes = d.arguments.map(_.symbol.ty)
        d.constructorSymbol.ty = Defs.types.Function.instanciate(new Types.Named(d.typeSymbol) :: argTypes)
        d.initSymbol.ty = Defs.types.Function.instanciate(Defs.types.Unit :: argTypes)
        val thisTy = new Types.Named(d.typeSymbol)
        d.thisSymbol.ty = if (d.isValue) Types.Qual.addAttribute(thisTy, Types.Attributes.Ref) else thisTy
      }

      case _ => ()
    }
    tree.children.filter { s => deep || (!s.scoped) }.foreach { (st => doTypeTree(st, deep)) }
    tree match {
      case e: Expr => typeExpr(e)
      case _ => ()
    }
  }

  def typeConversion(e: Expr, to: Type) = {

  }

  def canUseTypeFor(a: Type, b: Type): Boolean = (a.bareType, b.bareType) match {
    case (na: Types.Named, nb: Types.Named) => {
      na.typeSymbol == nb.typeSymbol
    }
    case (aa: Types.Named, _) => canUseTypeFor(aa.concreteType, b)
    case (_, bb: Types.Named) => canUseTypeFor(a, bb.concreteType)
    case (ca: ConcreteType, cb: ConcreteType) => compareConcreteTypes(ca, cb)
    case _ => compareConcreteTypes(a.concreteType, b.concreteType) //Utils.error("Huh? " + a + " -- " + b)
  }

  def compareConcreteTypes(a: ConcreteType, b: ConcreteType): Boolean = (a, b) match {
    case _ => a.typeSymbol == b.typeSymbol
  }

  def resolveTypeExpr(te: TypeExpr): Type = {
    te match {
      case tId: TypeId => { new Types.Named(tId.symbol) }
      case tFunc: TypeApply => {
        tFunc.symbol.definedType match {
          case tf: TypeFunctor => tf.instanciate(tFunc.args.map(resolveTypeExpr))
          case _ => Utils.error(tFunc.symbol + " isnt a type functor")
        }
      }
    }
  }

  def typeExpr(e: Expr): Unit = {
    e.ty = e match {
      case Block(es) => {
        es match { case Seq() => Defs.types.Unit case _ => es.last match { case ee: Expr => ee.ty case _ => Defs.types.Unit } }
      }

      case arg : Definition.Argument => {
        val ty = resolveTypeExpr(arg.typeExpr)
        if (arg.symbol.ty == null) {
          arg.symbol.ty = arg.mode match {
            case Definition.ArgModes.Copy => ty
            case Definition.ArgModes.Ref => Types.Qual.addAttribute(ty, Types.Attributes.Ref)
            case Definition.ArgModes.Readonly => Types.Qual.addAttribute(ty, Types.Attributes.Readonly)
          }
        }
        arg.symbol.ty
      }

      case mb @ Select(e, name) => {
        e.ty.memberSymbol(name) match {
          case None => Utils.error("No field named " + name + " in " + e.ty)
          case Some(sym) => {
            mb.fieldSymbol = sym
            e.ty.memberType(name).get
          }
        }
      }

      case PtrRef(ptr) => {
        Defs.types.Ptr.create(ptr.ty) //TODO check that "ptr" is mutable (and change "ptr" to "value") (and move this to a function @ of ptr)
      }

      case PtrDeref(ptr) => ptr.ty.concreteType match {
        case ptrTy: Defs.types.Ptr.Instance => { ptrTy.underlying }
        case _ => Utils.error("Trying to deref a non pointer type")
      }

      case Assign(lv, value) => {
        if (!canUseTypeFor(lv.ty, value.ty)) throw new RuntimeException("Expected " + lv.ty + " got " + value.ty)
        if (lv.ty.attributes.contains(Types.Attributes.Readonly)) Utils.error("Trying to assign readonly left-value of type " + lv.ty)
        lv match {
          case id: Id => id.symbol.definition match {
            case ValDefinition(_, _, _, _ /*true*/ ) => () // Had to change this cause of init in struct, TODO do better
            case _ : Definition.Argument => ()
            case d @ _ => Utils.error("Illegal assignement : " + d)
          }
          case PtrDeref(ptr) => {

          }
          case Select(e, name) => {

          }
          case _ => Utils.error("Illegal assignement")
        }
        Defs.types.Unit
      }

      case id: Id => {
        id.symbol.ty
      }

      case Call(f, args) => {
        val fType = f.ty.bareType match {
          case ft: Defs.types.Function.Instance => ft
          case _ => throw new RuntimeException("Attempt to call a non-function type (" + f.ty + ") " + f)
        }
        if (fType.argTypes.length != args.length) Utils.error("Wrong number of arguments for " + f + " : " + fType)
        val cmp = fType.argTypes.zip(args.map(_.ty)).map { case (t1, t2) => canUseTypeFor(t2, t1) }
        if (cmp.contains(false)) Utils.error("Wrong argument type for " + f)
        fType.retType
      }

      case If(cond, trueBlock, falseBlock) => {
        if (!canUseTypeFor(cond.ty, Defs.types.Bool)) throw new RuntimeException("Must be a bool")
        if (!canUseTypeFor(trueBlock.ty, falseBlock.ty)) throw new RuntimeException("Both expr must be of same type")
        trueBlock.ty
      }

      case While(cond, body) => {
        if (!canUseTypeFor(cond.ty, Defs.types.Bool)) throw new RuntimeException("Must be a bool")
        Defs.types.Unit
      }

      case lit: Literal[_] => lit.ty

      case vd: ValDefinition => {
        vd.valSymbol.ty = resolveTypeExpr(vd.valTypeExpr)
        val ok = vd.value match {
          case None => true
          case Some(v) => {
            if (!canUseTypeFor(v.ty, vd.valSymbol.ty))
              Utils.error("Expected " + vd.valSymbol.ty.typeSymbol + " got " + v.ty.bareType)
          }
        }
        Defs.types.Unit
      }
    }

  }

}

// Simple typer, inferrence + implicits someday ?
class TypePhase extends Phase[CompilationUnit, CompilationUnit] {

  def name = "type"

  def execute(cu: CompilationUnit): CompilationUnit = {
    Typer.typeTree(cu.root)
    cu
  }

}
*/ 