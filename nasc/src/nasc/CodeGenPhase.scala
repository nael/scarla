package nasc
import java.io.ByteArrayOutputStream

trait SymbolStorage {
  def asRaw: SymbolStorage.Raw = this match { case r: SymbolStorage.Raw => r case _ => Utils.error("Not a raw storage : " + this) }
  def asPtr: SymbolStorage.Ptr = this match { case r: SymbolStorage.Ptr => r case _ => Utils.error("Not a ptr storage : " + this) }
  def asIndex: SymbolStorage.Index = this match { case r: SymbolStorage.Index => r case _ => Utils.error("Not an indexed storage : " + this) }
}

object SymbolStorage {
  case class None extends SymbolStorage
  case class Raw(name: String) extends SymbolStorage

  case class Ptr(name: String) extends SymbolStorage
  case class Index(index: Int) extends SymbolStorage
}

class CodeGenPhase extends Phase[CompilationUnit, String] {

  def name = "codegen"

  def execute(cu: CompilationUnit): String = {
    val os = new ByteArrayOutputStream()
    val cg = new CodeGenerator(os)
    decideStorage(cg, cu.root)
    cg.prelude()
    val defs = findDefs(cu.root)
    defs.map(genDef(cg, _))
    cg.beginMain()
    genTree(cg, cu.root)
    cg.endMain()
    cg.end()
    os.toString()
  }

  def decideStorage(cg: CodeGenerator, tree: Tree): Unit = {
    tree.children.foreach(decideStorage(cg, _))
    tree match { case sd: SymDef => decideSymbolStorage(cg, sd) case _ => () }
  }

  def decideSymbolStorage(cg: CodeGenerator, sd: SymDef): Unit = {
    val typeSymbols = sd.symbols.filter(_.isType).toList
    typeSymbols.foreach { ts => ts.storage = SymbolStorage.Raw("%" + ts.uniqueName) }
    if (typeSymbols.length < sd.symbols.toList.length) {
      sd match {
        case ed: ExternFunDef => ed.funSymbol.storage = SymbolStorage.Raw(ed.llvmName)
        case vd: ValDefinition => vd.valSymbol.storage = SymbolStorage.Ptr(cg.freshName(vd.valSymbol.name))
        case fd: FunctionDefinition => {
          fd.funSymbol.storage = SymbolStorage.Raw("@" + fd.funSymbol.uniqueName)
        }
        case arg: Definition.Argument => {
          arg.symbol.storage = SymbolStorage.Ptr(cg.freshName(arg.symbol.name))
        }
        case b: BuiltinFunDef => { b.funSymbol.storage = SymbolStorage.None() }
        case sd: StructDefinition => {
          var fieldIdx = 0
          sd.body.children.foreach {
            case vd: ValDefinition => {
              val idx = sd.definedType.fields.filter(_.isInstanceOf[Types.Aggregate.Field]).findIndexOf(_.name == vd.name)
              vd.valSymbol.storage = SymbolStorage.Index(idx)
            }
            case _ => ()
          }
        }
      }
    }
  }

  def findDefs(tree: Tree): List[SymDef] = {
    (tree match {
      case sd: SymDef => List(sd)
      case _ => List()
    }) ++ tree.children.map(findDefs).flatten.toList
  }

  def genDef(cg: CodeGenerator, sd: SymDef) = {
    sd.symbols.foreach {
      case ts: TypeSymbol if ts.definedType != null =>
        if (!ts.definedType.isInstanceOf[TypeFunctor] && ts.definedType.concreteType != Defs.types.Unit) {
          cg.writer.println(ts.storage.asRaw.name + " = type " + ts.definedType.concreteType.llvmType)
        }
      case _ => ()
    }
    sd match {
      case fd: FunctionDefinition => genFunDef(cg, fd)
      case sd: StructDefinition => genStructDef(cg, sd)
      case ed: ExternFunDef if ed.redeclare => {
        cg.writer.println("declare " + ed.functionType.retType.llvmType + " " + ed.llvmName + "(" + Utils.repsep(ed.functionType.argTypes.map(_.llvmType)) + ")")
      }
      case _ => ()
    }
  }

  def genStructDef(cg: CodeGenerator, sd: StructDefinition) = {

  }

  def genFunDef(cg: CodeGenerator, fd: FunctionDefinition) = {
    val llvmArgNames = fd.arguments.map { a => cg.freshName(a.symbol.uniqueName) }
    val argString = llvmArgNames.zip(fd.functionType.argTypes).map { case (arg, ty) => ty.llvmType + " " + arg }
    val funName = fd.funSymbol.storage.asRaw.name
    cg.beginFunction(fd.functionType.retType.llvmType, funName, Utils.repsep(argString))
    fd.arguments.zip(fd.functionType.argTypes).zip(llvmArgNames).foreach {
      case ((arg, argTy), llvmName) =>
        val vd = ValDefinition(arg.symbol.name, null, Some(IRValue(llvmName)), true)
        vd.valSymbol = arg.symbol
        genExpr(cg, vd)
    }
    val res = genExpr(cg, fd.body)
    if (fd.functionType.retType.concreteType == Defs.types.Unit)
      cg.writer.println("ret void")
    else {
      cg.writer.println("ret " + fd.functionType.retType.llvmType + " " + res)
    }
    cg.endFunction()
  }

  def genTree(cg: CodeGenerator, tree: Tree) = {
    tree match {
      case _: BuiltinTypeDef => ()
      case _: BuiltinFunDef => ()
      case expr: Expr => genExpr(cg, expr)
      case fd: FunctionDefinition => ()
      case sd: StructDefinition => ()
      case ed: ExternFunDef => ()
      case td: TraitDefinition => ()
    }
  }

  def genLvaluePointer(cg: CodeGenerator, lv: LValue): String = lv match {
    case id: Id => { id.symbol.storage.asPtr.name }
    case PtrDeref(ptr) => {
      genExpr(cg, ptr)
    }
    case ma @ Select(e, ptr) => {

      val res = cg.freshName()
      val structTy = e.ty.bareType.concreteType.asInstanceOf[Types.Struct]
      if (structTy.isValueClass) {
        val llv = e match { case x: LValue => x }
        val eV = genLvaluePointer(cg, llv)
        cg.writer.println(res + " = getelementptr " + e.ty.llvmType + "* " + eV + ", i32 0,  i32 " + ma.fieldSymbol.storage.asIndex.index)
      } else {
        val eV = genExpr(cg, e)
        cg.writer.println(res + " = getelementptr " + e.ty.llvmType + " " + eV + ", i32 0,  i32 " + ma.fieldSymbol.storage.asIndex.index)
      }
      res
    }
  }

  def genExpr(cg: CodeGenerator, expr: Expr): String = {
    expr match {
      case IRValue(u) => u
      case Block(List()) => "undef"
      case Block(List(expr: Expr)) => genExpr(cg, expr)
      case Block(st :: es) => { genTree(cg, st); genExpr(cg, Block(es)) }
      case lit: Literal[_] => genLiteral(cg, lit)
      case vd: ValDefinition => {
        if (vd.valSymbol.ty != Defs.types.Unit) {
          val llvmName = vd.valSymbol.storage.asPtr.name
          cg.allocatePtr(llvmName, vd.valSymbol.ty.llvmType)
          vd.value match {
            case None => ()
            case Some(v) => {
              val value = genExpr(cg, v)
              cg.store(vd.valSymbol.ty.llvmType + "* " + llvmName, vd.valSymbol.ty.llvmType + " " + value)
            }
          }
        }
        "undef"
      }

      case mb @ Select(e, name) => {
        val res = cg.freshName()
        mb.fieldSymbol.storage match {
          case SymbolStorage.Raw(x) => x
          case SymbolStorage.Index(fieldIndex) => {
            val structTy = e.ty.bareType.concreteType.asInstanceOf[Types.Struct]
            structTy.fields.filter(_.isInstanceOf[Types.Aggregate.Field])(fieldIndex) match {
              case f: Types.Aggregate.Field => {
                e match {
                  case lv: LValue => { // This is to avoid loading an entire struct just to get one field if it is in fact a pointer
                    val resPtr = genLvaluePointer(cg, mb)
                    cg.writer.println(res + " = load " + f.symbol.ty.llvmType + "* " + resPtr)
                    res
                  }
                  case _ => {
                    val eV = genExpr(cg, e)
                    if (structTy.isValueClass)
                      cg.writer.println(res + " = extractvalue " + e.ty.llvmType + " " + eV + ", " + fieldIndex)
                    else {
                      Utils.error("NOT DONE YET")
                      cg.writer.println(res + " = getelementptr " + e.ty.llvmType + " " + eV + ", i32 0, i32 " + fieldIndex)
                    }
                    res
                  }
                }
              }
              case f: Types.Aggregate.Function => {
                Utils.error("Cannot (yet) access struct func ptr : " + mb)
              }
            }
          }
        }
      }

      case PtrDeref(e) => {
        val res = cg.freshName()
        val eV = genExpr(cg, e)
        cg.loadPtr(res, e.ty.llvmType + " " + eV)
        res
      }
      
      //TODO move this to a generic malloc[T]() when generics done
      case Call(id: Id, List(sizeof, ptr : Id)) if id.symbol == Defs.malloc => {
        val ptrV = ptr.symbol.storage.asPtr.name
        val sizeV = genExpr(cg, sizeof)
        val tmp = cg.freshName()
        val tmp2 = cg.freshName()
        val pTy = ptr.ty.llvmType
        cg.writer.println(tmp + " = call noalias i8* @malloc(i64 " + sizeV + ")")
        cg.writer.println(tmp2 + " = bitcast i8* " + tmp + " to " + pTy)
        cg.writer.println("store " + pTy + " " + tmp2 + ", " + pTy  + "* " + ptrV)
        "undef"
      } 
        

      case Call(id: Id, args) if id.symbol.definition.isInstanceOf[BuiltinFunDef] => {
        val builtin = id.symbol.definition.asInstanceOf[BuiltinFunDef].fun
        val argsValues = args.map(genExpr(cg, _))
        builtin.genBody(cg, argsValues)
      }

      case call: Call => {
        val funType = call.f.ty.bareType.concreteType match { case t: Defs.types.Function.Instance => t case _ => throw new RuntimeException("Not a function type " + call.f.ty) }
        val funcValue = genExpr(cg, call.f)
        val argsValue = call.args.map { arg => arg.ty.llvmType + " " + genExpr(cg, arg) }
        val res = cg.freshName()
        val llvmFun = funType.retType.llvmType + " " + funcValue
        if (funType.retType.concreteType == Defs.types.Unit) {
          cg.voidFunctionCall(llvmFun, argsValue)
          "undef"
        } else {
          cg.functionCall(llvmFun, argsValue, res)
          res
        }

      }

      case Assign(lvalue, value) => {
        val v = genExpr(cg, value)

        val llvmName = genLvaluePointer(cg, lvalue)
        cg.store(lvalue.ty.llvmType + "* " + llvmName, lvalue.ty.llvmType + " " + v)
        "undef"
      }

      case If(condition, trueBlock, falseBlock) => {
        val condV = genExpr(cg, condition)
        val trueLabel = cg.freshLabel("true_label")
        val falseLabel = cg.freshLabel("false_label")
        val endLabel = cg.freshLabel("end_if_label")
        val resultV = cg.freshName()
        cg.writer.println("br i1 " + condV + ", label %" + trueLabel + ", label %" + falseLabel)
        cg.writer.println(trueLabel + ":")
        val tbV = genExpr(cg, trueBlock)
        cg.writer.println("br label %" + endLabel)
        cg.writer.println(falseLabel + ":")
        val fbV = genExpr(cg, falseBlock)
        cg.writer.println("br label %" + endLabel)
        cg.writer.println(endLabel + ":")
        cg.writer.println(resultV + " = phi " + trueBlock.ty.llvmType + " [" + tbV + ", %" + trueLabel + "], [" + fbV + ", %" + falseLabel + "]")
        resultV
      }

      case While(cond, body) => {
        val testLabel = cg.freshLabel("test_label")
        val loopBodyLabel = cg.freshLabel("loop_body")
        val endLoopLabel = cg.freshLabel("end_loop")
        cg.writer.println("br label %" + testLabel)
        cg.writer.println(testLabel + ":")
        val condV = genExpr(cg, cond)
        cg.writer.println("br i1 " + condV + ", label %" + loopBodyLabel + ", label %" + endLoopLabel)
        cg.writer.println(loopBodyLabel + ":")
        genExpr(cg, body)
        cg.writer.println("br label %" + testLabel)
        cg.writer.println(endLoopLabel + ":")
        "undef"
      }

      case PtrRef(lv: LValue) => {
        genLvaluePointer(cg, lv)
      }

      case id: Id => {
        if (id.ty == Defs.types.Unit) "undef"
        id.symbol.storage match {
          case SymbolStorage.None() => throw new RuntimeException("Cant access " + id)
          case SymbolStorage.Raw(x) => x
          case SymbolStorage.Ptr(x) => {
            val valName = cg.freshName(id.name + "_val")
            cg.loadPtr(valName, id.ty.llvmType + "* " + x)
            valName
          }
          case null => Utils.error("No storage for : " + id)
        }
      }

    }
  }

  def genLiteral(cg: CodeGenerator, lit: Literal[_]) = lit match {
    case Literals.Boolean(true) => "1"
    case Literals.Boolean(false) => "0"
    case Literals.Integer(v) => v.toString
    case Literals.String(s) => Utils.error("No string lit yet")
  }

}