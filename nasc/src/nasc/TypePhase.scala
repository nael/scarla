package nasc

object Typer {
  def typeTree(s: Tree) = {
    doTypeTree(s, true)
  }

  def typed(s: List[Tree]): List[Tree] = {
    s.map { st => typed(st) }
  }

  def typed[T <: Tree](s: T): T = {
    typeTree(s)
    s
  }

  def collectAggregateFields(d: AggregateDefinition[_]): List[Types.Aggregate.Element] = {
    val defBody = d.body.duplicate()
    defBody.children.map(doTypeTree(_, false)) // Ensure we know the fields types
    defBody.children.toList.reverse.foldLeft(List[Types.Aggregate.Element]()) {
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
      case _ => ()
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
        es match { case List() => Defs.types.Unit case _ => es.last match { case ee: Expr => ee.ty case _ => Defs.types.Unit } }
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
