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
      val valName = cg.freshName(name + "_val")
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
      else if (s == Builtin.Boolean.symbol) "i1"
      else if (s == Builtin.CPtr.symbol) "i8*" // TODO store this correspondance somewhere else
      else if (Builtin.isFunction(s)) {
        typeLayout(Builtin.functionReturnType(s)) + "(" + (Builtin.functionArgTypes(s) map typeName mkString ", ") + ")*"
      } else s.definition match {
        case td: TypeDef => td.value match {
          case Some(st: Struct) => {
            val vs = s.typeInfo.vals
            "{" + (s.typeInfo.vals map { sym => typeName(sym.typeSymbol) } mkString ", ") + "}"
          }
          case Some(uu: Sym) => typeLayout(uu.symbol)
          case _ => Utils.error("No layout for type " + s)
        }
        case _ => Utils.error("Trying to layout type defined at : " + s.definition)
      }
    layout + (if (s.definition.hasAttr[attributes.Move]) "*" else "")
  }

  def execute(rawTree: Tree): String = {
    val os = new ByteArrayOutputStream()
    val cg = new CodeGenerator(os)
    
    val tree = TreeUtils.simplifyBlocks(rawTree)
    
    tree traverse { case td: TypeDef if td.value != None => td.typeName.symbol.storage = SymbolStorage.Raw(cg.freshName(td.typeName.symbol.uniqueName)) }
    tree traverse {
      case td: TypeDef if td.value != None =>
        cg.writer.println(td.typeName.symbol.storage.asRaw.name + " = type " + typeLayout(td.typeName.symbol))
    }
    tree.children foreach { //Global val's
      case vd: ValDef => {
        val sym = vd.valName.symbol
        val llvmName = cg.freshGlobal(sym.uniqueName)
        val llvmType = typeName(sym.typeSymbol)
        sym.storage = SymbolStorage.Ptr(llvmName, llvmType)
        cg.writer.println(llvmName + " = global " + llvmType + " undef")        
      }
      case _ => ()
    }

    cg.prelude()
    decideStorage(cg, tree)
    genDefs(cg, tree)
    /*cg.beginMain()
    genTree(cg, tree)
    cg.endMain()*/
    cg.end()
    os.toString()
  }

  def decideStorage(cg: CodeGenerator, tree: Tree) = {
    tree traverse {

      case dd: DefDef => {
        val nat = dd.attr collect { case attributes.Native(x) => x }
        val llvmFunName = if (nat.isEmpty) {
          if (dd.body.isEmpty) Utils.error("Non native function with empty body " + dd.defName)
          cg.freshGlobal(dd.defName.symbol.uniqueName)
        } else {
          "@" + nat.head
        }
        dd.defName.symbol.storage = SymbolStorage.Raw(llvmFunName)
      }

      case td: TypeDef => {
        td.value match {
          case Some(s: Struct) => {
            td.typeName.symbol.typeInfo.vals.zipWithIndex foreach {
              case (x, i) => {
                x.storage = SymbolStorage.Index(i)
              }
            }
          }
          case _ => ()
        }
      }

    }
  }

  def genDefs(cg: CodeGenerator, tree: Tree) = {
    tree traverse {

      case dd: DefDef if !dd.body.isEmpty => {

        val fun = dd.defName.symbol
        val llvmFunName = fun.storage.asRaw.name
        val args = dd.arguments map { _.argName.symbol }
        val llvmArgValNames = args map { arg => cg.freshName(arg.uniqueName + "_arg") }
        val llvmArgNames = args map { arg => cg.freshName(arg.uniqueName) }

        val argTypes = Builtin.functionArgTypes(fun.typeSymbol)
        val retType = Builtin.functionReturnType(fun.typeSymbol)

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
          case _ => Utils.error("Select storage : " + mSym.storage + " for " + sel.memberName)
        }
      }
      case _ => Utils.error("Cannot generate pointer to " + tree)
    }
  }

  // Generates code to store the value carried by @tree in an llvm register which name is returned
  // If @tree is of type @sym, the register will be typed typeName(@sym) in llvm
  def genTree(cg: CodeGenerator, tree: Tree): String = {
    tree match {

      case b: Block if !b.children.isEmpty => b.children map { t => genTree(cg, t) } last
      case _: Block => "undef"

      case s: Sym if s.symbol.storage != null =>  s.symbol.storage.genAccess(cg)
      case s: Sym => Utils.error("Symbol " + s.symbol + " has no storage")

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

      case n: New => {
        //Utils.assert(n.typeSymbol.definition.hasAttr[attributes.Move])
        // TODO think about the different needed types of alloc
        if (n.typeSymbol.definition.hasAttr[attributes.Heap]) {
          val sizeof0 = cg.freshName("sz")
          val sizeof1 = cg.freshName("sz")
          cg.writer.println(sizeof0 + " = getelementptr " + typeName(n.typeSymbol) + " null, i64 1")
          cg.writer.println(sizeof1 + " = ptrtoint " + typeName(n.typeSymbol) + " " + sizeof0 + " to i64")
          val nptr = cg.freshName()
          cg.functionCall("i8* @malloc", Seq("i64 " + sizeof1), nptr)
          //cg.voidFunctionCall("void @printInt", Seq("i32 " + sizeof1)) 
          val v = cg.freshName()
          cg.writer.println(v + " = bitcast i8* " + nptr + " to " + typeName(n.typeSymbol))
          v
        } else "undef"
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

      case ta: cast.TypeAttr => {
        var v = ""
        ta.add foreach {
          case attributes.Move() => {
            v = genPointer(cg, ta.tree)
          }
          case attr => Utils.error("Unsupported cast : +" + attr)
        }
        ta.remove foreach {
          case attr => Utils.error("Unsupported cast : -" + attr)
        }
        v
      }
      
      case bc: cast.BitCast => {
        val ptr = genTree(cg, bc.ptr)
        val fromType = typeName(bc.ptr.typeSymbol)
        val destType = typeName(bc.typeTree.symbol)
        val c = cg.freshName()
        cg.writer.println(c + " = bitcast " + fromType + " " + ptr + " to " + destType)
        c
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

      case i: If => {
        val condV = genTree(cg, i.condition)
        val trueLabel = cg.freshLabel("true_label")
        val falseLabel = cg.freshLabel("false_label")
        val endLabel = cg.freshLabel("end_if_label")
        val resultV = cg.freshName()
        cg.writer.println("br i1 " + condV + ", label %" + trueLabel + ", label %" + falseLabel)
        cg.beginBlock(trueLabel)
        val tbV = genTree(cg, i.ifTrue)
        cg.writer.println("br label %" + endLabel)
        val trueReturnBlock = cg.currentBlock
        cg.beginBlock(falseLabel)
        val fbV = genTree(cg, i.ifFalse)
        cg.writer.println("br label %" + endLabel)
        val falseReturnBlock = cg.currentBlock
        cg.beginBlock(endLabel)
        if (i.ifTrue.typeSymbol != Builtin.Unit.symbol) {
          cg.writer.println(resultV + " = phi " + typeName(i.ifTrue.typeSymbol) + " [" + tbV + ", %" + trueReturnBlock + "], [" + fbV + ", %" + falseReturnBlock + "]")
          resultV
        } else "undef"
      }

      case w: While => {
        val testLabel = cg.freshLabel("test_label")
        val loopBodyLabel = cg.freshLabel("loop_body")
        val endLoopLabel = cg.freshLabel("end_loop")
        cg.writer.println("br label %" + testLabel)
        cg.beginBlock(testLabel)
        val condV = genTree(cg, w.condition)
        cg.writer.println("br i1 " + condV + ", label %" + loopBodyLabel + ", label %" + endLoopLabel)
        cg.beginBlock(loopBodyLabel)
        genTree(cg, w.body)
        cg.writer.println("br label %" + testLabel)
        cg.beginBlock(endLoopLabel)
        "undef"
      }

      case lit: Literal => {
        lit.value match {
          case i: Int => i.toString
          case false => "0"
          case true => "1"
          case null => "undef"
        }
      }

      case _ => { "undef" }
    }
  }
}
