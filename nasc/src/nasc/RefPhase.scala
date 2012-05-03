package nasc

class RefPhase extends Phase[CompilationUnit, CompilationUnit] {
  def name = "ref"

  def execute(cu: CompilationUnit): CompilationUnit = {
    cu.root = cu.root.transform(transformCalls)
    cu.root = cu.root.transform(transformDefs)
    cu.root = cu.root.transform(simplifyRefDeref)
    cu.root = cu.root.transform(removeRefTypes)
    cu
  }

  def removeRefTypes(st: Statement): Statement = st match {
    case e: Expr => { println("E " + e); e.ty = Types.Qual.removeAttribute(e.ty, Types.Attributes.Ref); e }
    case _ => st
  }

  def simplifyRefDeref(st: Statement): Statement = st match {
    case PtrRef(PtrDeref(u)) => u
    case PtrDeref(PtrRef(u)) => u
    case _ => st
  }

  def transformCalls(st: Statement): Statement = st match {
    case call: Call => {
      call.f.ty.bareType match {
        case functionType: Defs.types.Function.Instance => {
          val newArgs = call.args.zip(functionType.argTypes).map {
            case (arg, refTy) if refTy.isRef => {
              val pref = PtrRef(arg)
              pref.ty = Defs.types.Ptr.create(refTy.bareType)
              pref
            }
            case (arg, _) => arg
          }
          val nc = Call(call.f, newArgs)
          nc.ty = call.ty
          nc
        }
        case _ => Utils.error("qosidjqoisjd " + call.f.ty)
      }
    }
    case _ => st
  }

  def transformDefs(st: Statement): Statement = st match {
    case fd: FunctionDefinition if fd.args.exists { a => a.symbol.ty.isRef } => {
      val refArgs = fd.args.filter { a => a.symbol.ty.isRef}
      fd.transform(transformDef(refArgs.map(_.symbol), _))
    }
    case _ => st
  }

  def transformDef(refArgs: List[Symbol], st: Statement): Statement = st match {
    case id: Id if (refArgs.contains(id.symbol)) => {
      val pderef = PtrDeref(id)
      pderef.ty = id.ty
      if (id.ty.attributes.contains(Types.Attributes.Ref)) {
        id.ty = Defs.types.Ptr.create(id.ty.bareType)
      }
      pderef
    }
    case _ => st
  }

}