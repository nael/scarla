package nasc
import java.io.ByteArrayOutputStream

trait SymbolStorage {

  def genAccess(cg: CodeGenerator): String

  def asRaw: SymbolStorage.Raw = this match { case r: SymbolStorage.Raw => r case _ => Utils.error("Not a raw storage : " + this) }
  def asPtr: SymbolStorage.Ptr = this match { case r: SymbolStorage.Ptr => r case _ => Utils.error("Not a ptr storage : " + this) }
  def asIndex: SymbolStorage.Index = this match { case r: SymbolStorage.Index => r case _ => Utils.error("Not an indexed storage : " + this) }
}

object SymbolStorage {

  case class None extends SymbolStorage {
    def genAccess(cg: CodeGenerator): String = Utils.error("Cant access None storage")
  }

  case class Raw(name: String) extends SymbolStorage {
    def genAccess(cg: CodeGenerator) = name
  }

  case class Ptr(name: String, ty: String) extends SymbolStorage {
    def genAccess(cg: CodeGenerator) = {
      val valName = cg.freshLabel(name + "_val")
      cg.loadPtr(valName, ty + "* " + name)
      valName
    }
  }
  case class Index(index: Int) extends SymbolStorage {
    def genAccess(cg: CodeGenerator): String = Utils.error("Cant access Index storage without context")
  }
}

class CodeGenPhase extends Phase[Tree, String] {
  def name = "codegen"

  //var _typeLayoutCache = Map[Symbol, String]()
  def typeName(s: Symbol): String = {
    Utils.assert(s.isType)
    s.storage match {
      case SymbolStorage.Raw(x) => x
      case _ => typeLayout(s)
    }
  }
  def typeLayout(s: Symbol): String = {
    Utils.assert(s.isType)
    val layout =
      if (s == Builtin.Int.symbol) "i32"
      else if (s == Builtin.Unit.symbol) "void"
      else if (Builtin.isFunction(s)) {
        typeLayout(Builtin.functionReturnType(s)) + "(" + (Builtin.functionArgTypes(s) map typeName mkString ", ") + ")*"
      } else s.definition match {
        case td: TypeDef => td.value match {
          case Some(st: Struct) => {
            val vs = s.typeInfo.vals
            "{" + (s.typeInfo.vals map { sym => typeName(sym.typeSymbol) } mkString ", ") + "}"
          }
          case _ => Utils.error("No layout for type " + s)
        }
        case _ => Utils.error("Trying to layout type defined at : " + s.definition)
      }
    layout + (if (s.definition.hasAttr[attributes.Move]) "*" else "")
  }

  def execute(tree: Tree): String = {
    val os = new ByteArrayOutputStream()
    val cg = new CodeGenerator(os)

    tree traverse { case td: TypeDef if td.value != None => td.typeName.symbol.storage = SymbolStorage.Raw(cg.freshName(td.typeName.symbol.uniqueName)) }
    tree traverse {
      case td: TypeDef if td.value != None =>
        cg.writer.println(td.typeName.symbol.storage.asRaw.name + " = type " + typeLayout(td.typeName.symbol))
    }

    cg.prelude()
    genDefs(cg, tree)
    cg.beginMain()
    genTree(cg, tree)
    cg.endMain()
    cg.end()
    os.toString()
  }

  def genDefs(cg: CodeGenerator, tree: Tree) = {
    tree traverse {

      case dd: DefDef if !dd.body.isEmpty => {
        println("Codegen fun " + dd.defName.name)

        val fun = dd.defName.symbol
        val llvmFunName = cg.freshGlobal(fun.uniqueName)
        val args = dd.arguments map { _.argName.symbol }
        val llvmArgValNames = args map { arg => cg.freshName(arg.uniqueName + "_arg") }
        val llvmArgNames = args map { arg => cg.freshName(arg.uniqueName) }

        val argTypes = Builtin.functionArgTypes(fun.typeSymbol)
        val retType = Builtin.functionReturnType(fun.typeSymbol)

        fun.storage = SymbolStorage.Raw(llvmFunName)
        args zip llvmArgNames zip argTypes foreach { case ((arg, llvmArgName), argType) => arg.storage = SymbolStorage.Ptr(llvmArgName, typeName(argType)) }

        val argString = argTypes zip llvmArgValNames map { case (argType, llvmArgValName) => typeName(argType) + " " + llvmArgValName } mkString ", "

        cg.beginFunction(typeName(retType), llvmFunName, argString)

        llvmArgNames zip llvmArgValNames zip argTypes foreach {
          case ((llvmArgName, llvmArgValName), argType) =>
            cg.allocatePtr(llvmArgName, typeName(argType))
            cg.store(typeName(argType) + "* " + llvmArgName, typeName(argType) + " " + llvmArgValName)
        }

        val res = genTree(cg, dd.body.get)
        if (retType == Builtin.Unit.symbol)
          cg.writer.println("ret void")
        else {
          cg.writer.println("ret " + typeName(retType) + " " + res)
        }
        cg.endFunction()
      }

      case dd: DefDef => {
        val nat = dd.attr collect { case attributes.Native(x) => x }
        if (nat.isEmpty) Utils.error("Non-native function with no body made it through codegen : " + dd)
        dd.defName.symbol.storage = SymbolStorage.Raw("@" + nat.head)
      }

      case td: TypeDef => {
        td.value match {
          case Some(s: Struct) => {
            td.typeName.symbol.typeInfo.vals.zipWithIndex foreach {
              case (x, i) => {
                println("Choosing index(" + i + ") for " + x)
                x.storage = SymbolStorage.Index(i)
              }
            }
          }
          case _ => ()
        }
      }

    }
  }

  def genPointer(cg: CodeGenerator, tree: Tree): String = {
    tree match {
      case s: Sym => s.symbol.storage.asPtr.name
      case sel: Select => {

        val mSym = sel.memberName.symbol
        val res = cg.freshName()
        mSym.storage match {
          case SymbolStorage.Index(i) => {
            val fromType = typeName(sel.from.typeSymbol)
            val from = if (sel.from.typeSymbol.definition.hasAttr[attributes.Move]) {
              fromType + " " + genTree(cg, sel.from)
            } else {
              fromType + "* " + genPointer(cg, sel.from)
            }
            cg.writer.println(res + " = getelementptr " + from + ", i32 0,  i32 " + i)
            res

          }
          case _ => Utils.error("q2sdqsd")
        }
      }
      case _ => Utils.error("Cannot generate pointer to " + tree)
    }
  }

  def genTree(cg: CodeGenerator, tree: Tree): String = {
    tree match {

      case b: Block => b.children map { t => genTree(cg, t) } last

      case s: Sym => { println("Gen$ " + s); s.symbol.storage.genAccess(cg) }

      case a: Apply => {
        val fTy = a.function.typeSymbol
        val retTy = fTy.typeVars.last._2
        val f = typeName(retTy) + " " + genTree(cg, a.function)
        val args = a.arguments map { arg => typeName(arg.typeSymbol) + " " + genTree(cg, arg) }
        if (retTy == Builtin.Unit.symbol) {
          cg.voidFunctionCall(f, args)
          "undef"
        } else {
          val res = cg.freshName()
          cg.functionCall(f, args, res)
          res
        }

      }

      case vd: ValDef => {
        val sym = vd.valName.symbol
        if (sym.typeSymbol != Builtin.Unit) {
          val llvmName = cg.freshName(sym.uniqueName)
          val llvmType = typeName(sym.typeSymbol)
          sym.storage = SymbolStorage.Ptr(llvmName, llvmType)
          cg.allocatePtr(llvmName, llvmType)
          vd.value match {
            case None => ()
            case Some(v) => {
              val value = genTree(cg, v)
              cg.store(llvmType + "* " + llvmName, llvmType + " " + value)
            }
          }
        } else { vd.valName.symbol.storage = SymbolStorage.None() }
        "undef"
      }

      case sel: Select => {
        //TODO handle sel.from is NOT an lvalue
        val ptr = genPointer(cg, sel)
        val res = cg.freshName()
        cg.writer.println(res + " = load " + typeName(sel.memberName.typeSymbol) + "* " + ptr)
        res
      }

      case a: Assign => {
        val v = genTree(cg, a.value)
        val ptr = genPointer(cg, a.dest)
        val vty = typeName(a.value.typeSymbol)
        cg.store(vty + "* " + ptr, vty + " " + v)
        "undef"
      }

      case lit: Literal[_] => {
        lit.value match {
          case i: Int => i.toString
        }
      }

      case _ => { println("Warning skipping codegen for " + tree.getClass); "undef" }
    }
  }
}
/*
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

}*/ 