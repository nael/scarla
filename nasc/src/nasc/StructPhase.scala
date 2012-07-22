package nasc

class StructPhase extends Phase[CompilationUnit, CompilationUnit] {
  def name = "struct"
  var defs: List[StructDefinition] = List()
  def execute(cu: CompilationUnit): CompilationUnit = {
    cu.root = cu.root.transform(addInit)
    cu.root = cu.root.transform(addCtor)
    cu.root = cu.root.transform(transformMethods)

    cu.root = cu.root.transform(transformMethodCalls)
    defs.foreach { d => cu.root = cu.root.transform(optimizeConstructorCalls(d)) }
    cu
  }

  def optimizeConstructorCalls(sd: StructDefinition)(st: Statement): Statement = st match {
    case Assign(lv, Call(fId: Id, args)) if fId.symbol == sd.constructorSymbol => {
      initCall(sd, lv :: args) //TODO remove initCall
    }
    case _ => st
  }

  def transformMethodCalls(st: Statement): Statement = st match {
    case call @ Call(mb @ Select(e, name), args) => {
      val id = Id.fromSymbol(mb.fieldSymbol)
      e.ty.bareType.concreteType match {
        case structType: Types.Struct => {
          val wrap = (structType.symbolMember(id.symbol).exists(_.isInstanceOf[Types.Aggregate.Function])) || id.symbol == structType.initSymbol //TODO prettier
          if (wrap) {
            val newCall = Call(id, e :: args)
            Typer.typed(newCall)
          } else call
        }
        case _ => call
      }

    }

    case _ => st
  }

  def transformMethods(st: Statement): Statement = st match {
    case sd: StructDefinition =>
      sd.transform(addParam(sd, _))
    case _ => st
  }

  def initCall(sd: StructDefinition, args: List[Expr]) = {
    val initId = Id.fromSymbol(sd.initSymbol)
    // Gotta do this here cause it wont be done by this phase : we are not calling init via a Select()
    val initTy = sd.initSymbol.ty.asInstanceOf[Defs.types.Function.Instance]
    initId.ty = Defs.types.Function.create(initTy.retType, sd.thisSymbol.ty :: initTy.argTypes)
    Call(initId, args)
  }

  def addInit(st: Statement): Statement = st match {
    case sd: StructDefinition => {
      defs ::= sd
      //sd.thisSymbol.ty = Defs.types.Ref.create(sd.thisSymbol.ty)

      // Add code for $init
      val initBody = sd.body.children.flatMap {
        case vd: ValDefinition if vd.value != None => {
          val valId = Id(vd.name)
          valId.symbol = vd.valSymbol
          List(Assign(valId, vd.value.get))
        }
        case vd: ValDefinition if vd.value == None => List()
        case _: FunctionDefinition => List()
        case st: Statement => List(st)
      }.toList
      val initDef = FunctionDefinition(sd.initSymbol.uniqueName, sd.constructorArguments, Typer.typed(Block(initBody)), TypeId.fromSymbol(Defs.types.Unit.typeSymbol))
      initDef.funSymbol = sd.initSymbol
      initDef.returnType = Defs.types.Unit
      val emptiedBody = sd.body.children.toList //.filter { case _ : FunctionDefinition => true case _ => false } toList

      val newSd: StructDefinition = sd.copy(body = Typer.typed(Block(initDef :: emptiedBody)))
      sd.copyAttrs(newSd.asInstanceOf[sd.type])
      newSd
    }
    case _ => st
  }

  def addCtor(s: Statement): Statement = s match {
    case sd: StructDefinition => {
      // Essentialy : { val a : A; a.$init(); a }
      val sv = ValDefinition("x", TypeId.fromSymbol(sd.typeSymbol), None, false)
      sv.declareSymbols()
      val id = Id.fromSymbol(sv.valSymbol)

      val alloc = if (sd.isValue) None else {
        val mallocCall = Call(Id.fromSymbol(Defs.malloc), List(Literals.Integer(88), id))
        mallocCall.ty = new Types.Named(sd.typeSymbol)
        Some(mallocCall)
      }

      //sv.valSymbol.ty = sd.typeSymbol.definedType

      //val call = initCall(sd, id :: (sd.constructorArguments.map { arg => Id.fromSymbol(arg.symbol) }))
      val init = Select(id, sd.name + "::init")
      init.fieldSymbol = sd.initSymbol
      init.ty = sd.initSymbol.ty
      val call = Call(init, sd.constructorArguments.map { arg => Id.fromSymbol(arg.symbol) })
      val ctorBody = sv :: (alloc.toList ++ List(call, id))
      val ctorDef = FunctionDefinition(sd.constructorSymbol.name, sd.constructorArguments, Typer.typed(Block(ctorBody)), TypeId.fromSymbol(sd.typeSymbol))
      ctorDef.funSymbol = sd.constructorSymbol
      ctorDef.returnType = new Types.Named(sd.typeSymbol)
      Typer.typeStatement(ctorDef)
      val b = Block(List(sd, ctorDef))
      b.ty = Defs.types.Unit
      b
    }
    case _ => s
  }

  def isSymbolToBeWrapped(sd: StructDefinition, s: Symbol) = {
    (s == sd.constructorSymbol || s == sd.initSymbol || sd.definedType.symbolMember(s) != None)
  }

  def addParam(sd: StructDefinition, st: Statement): Statement = st match {
    case fd: FunctionDefinition => {
      val thisArg = Definition.Argument("this", TypeId.fromSymbol(sd.thisSymbol.ty.typeSymbol), Definition.ArgModes.Copy)
      val newFd = FunctionDefinition(fd.name, thisArg :: fd.args, fd.body, fd.retTypeExpr)
      thisArg.symbol = sd.thisSymbol
      newFd.funSymbol = fd.funSymbol
      Typer.typed(newFd)
    }
    case id: Id => {
      if(isSymbolToBeWrapped(sd, id.symbol)) {
      val thisId = Id("this")
      thisId.symbol = sd.thisSymbol
      val ma = Select(thisId, id.name)
      ma.fieldSymbol = id.symbol
      Typer.typeStatement(ma)
      ma
      } else id
    }
    case _ => st
  }
}