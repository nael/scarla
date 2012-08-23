package nasc
import java.io.PrintWriter
import java.io.{ OutputStream, FileOutputStream }
import java.io.ByteArrayOutputStream

object CodeGenerator {
  def toFile(fileName: String): CodeGenerator = {
    val os = new FileOutputStream(fileName)
    new CodeGenerator(os)
  }
}

object BinOps extends Enumeration {
  type BinOp = Value
  val IAdd, ISub, ICmp, IMul, INeq, ILte = Value

  def llvmOpName(op: Value) = op match {
    case IAdd => "add i32"
    case ISub => "sub i32"
    case IMul => "mul i32"
    case ICmp => "icmp eq i32"
    case INeq => "icmp ne i32"
    case ILte => "icmp sle i32"
  }
}

class CodeGenerator(val dest: OutputStream) {

  var currentBlock = "[!!!]"
  val varNames = new collection.mutable.HashMap[String, Int]()
  val writer = new PrintWriter(dest)

  def freshName(s: String = ""): String = {
    "%" + freshLabel(s)
  }

  def freshGlobal(s: String = ""): String = {
    "@" + freshLabel(s)
  }

  def freshLabel(s: String) = {
    val ss = s.replace("%", "").replace("@", "")
    val i = varNames.getOrElse(ss, 0)
    varNames(ss) = i + 1
    ss + (if (i == 0 && ss != "") "" else "_" + i.toString())
  }

  def assign(a: String, b: String) = {
    writer.println(a + " = " + b)
  }

  def constantInt(value: Int, varName: String) = {
    writer.println(varName + " = or i32 " + value + ", 0")
  }

  def binOp(op: BinOps.Value, a: String, b: String, res: String) = {
    writer.println(res + " = " + BinOps.llvmOpName(op) + " " + a + ", " + b)
  }

  def extractValue(struct: String, idx: Int, res: String) = {
    writer.println(res + " = extractvalue " + struct + ", " + idx)
  }

  def allocatePtr(ptr: String, ty: String) = {
    writer.println(ptr + " = alloca " + ty)
  }

  def store(into: String, value: String) = {
    writer.println("store " + value + ", " + into)
  }

  def loadPtr(into: String, from: String) = {
    writer.println(into + " = load " + from)
  }

  def printInt(varName: String) = {
    writer.println("""call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.intFmtStr, i32 0, i32 0), i32 %""" + varName + ")")
  }

  def functionCall(funcName: String, args: Seq[String], res: String) = {
    writer.println(res + " = call " + funcName + "(" + Utils.repsep(args) + ")")
  }
  
  def beginBlock(name: String) = {
    currentBlock = name
    writer.println(name + ":")
  }

  def voidFunctionCall(funcName: String, args: Seq[String]) = {
    writer.println("call " + funcName + "(" + Utils.repsep(args) + ")")
  }
  def beginFunction(retType: String, name: String, args: String) = {
    writer.println("define " + retType + " " + name + "(" + args + ") nounwind {")
  }
  def endFunction() = {
    writer.println("}")
  }
  def prelude() = {
    writer.write("""""")
  }

  def beginMain() = {
    writer.println("define i32 @main(i32 %argc, i8** nocapture %argv) nounwind {")
  }

  def endMain() = {
    writer.write("""
ret i32 0
}
""")
  }
  def end() = {
    writer.close()
  }
}