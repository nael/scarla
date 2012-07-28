package nasc
/*
trait BuiltinFunction {
  def name: String
  def functionType: Defs.types.Function.Instance
  def genBody(cg: CodeGenerator, args: List[String]): String
}

object Builtin {
  def binOp(name : String, op : BinOps.BinOp, operandType: Type, resultType: Type) =
    BuiltinFunDef(BuiltinBinOp(name, op, operandType, resultType))
}

case class BuiltinBinOp(name: String, op: BinOps.BinOp, operandType: Type, resultType: Type) extends BuiltinFunction {
  def functionType = Defs.types.Function.create(resultType, List(operandType, operandType))
  def genBody(cg: CodeGenerator, args: List[String]) = args match {
    case List(leftOp, rightOp) => {
      val res = cg.freshName()
      cg.binOp(op, leftOp, rightOp, res)
      res
    }
    case _ => throw new RuntimeException("plqpsodkq")
  }
}

class BuiltinType(val name: String, val llvmType: String) extends SimpleType {
  val typeSymbol = new TypeSymbol(name, Defs.builtinTypeDef)
  typeSymbol.definedType = this
}

object Defs {
  val builtinTypeDef = BuiltinTypeDef()

  val malloc = new IdSymbol("malloc", null)
  malloc.ty = Defs.types.Function.create(Defs.types.Unit, List(Defs.types.Int, Defs.types.Ptr.create(Defs.types.Byte)))
  
  object types {
    val Int = new BuiltinType("Int", "i32")
    val Bool = new BuiltinType("Bool", "i1")
    val Unit = new BuiltinType("Unit", "void")
    val Byte = new BuiltinType("Byte", "i8")
    val Unknown = new SimpleType {
      val name = "<unknown>"
      val typeSymbol = new TypeSymbol("<unknown>", Defs.builtinTypeDef)
      def llvmType = Utils.error("Unknown type encoutered")
    }
    object Function extends SimpleTypeFunctor {
    
      val name = "Function"
      
      class Instance(
        val typeSymbol: TypeSymbol,
        val retType: Type,
        val argTypes: List[Type]) extends SimpleType {

        val name = (argTypes match { case List(x) => x.name case _ => "(" + Utils.repsep(argTypes.map(_.name)) + ")" }) + "->" + retType.name
        def llvmType = retType.llvmType + " (" + Utils.repsep(argTypes.map(_.llvmType)) + ")*"

      }

      val typeSymbol = new TypeSymbol("Function", Defs.builtinTypeDef)
      typeSymbol.definedType = this
      
      def create(ret : Type, args : List[Type]) = {
        instanciate(ret::args).asInstanceOf[Instance]
      }
      
      def instanciate(symbol: TypeSymbol, types: List[Type]): Instance = {
        new Instance(symbol, types.head, types.tail)
      }
    }
    
    object Ptr extends SimpleTypeFunctor {
      val name = "Ptr"
        
      class Instance(
        val typeSymbol: TypeSymbol,
        val underlying : Type
      ) extends SimpleType {
        val name = underlying.name + "*"
        def llvmType = underlying.llvmType + "*"
      }
      
      val typeSymbol = new TypeSymbol("Ptr", Defs.builtinTypeDef)
      typeSymbol.definedType = this
      
      def instanciate(symbol: TypeSymbol, types: List[Type]): Instance = {
        new Instance(symbol, types.head)
      }
      
      def create(ty : Type) : Instance = {
        instanciate(List(ty)).asInstanceOf[Instance]
      }
    }

    val list = List(Int, Bool, Unit, Byte, Unknown, Function, Ptr)
  }
  
  object functions {
    def plus = Builtin.binOp("+", BinOps.IAdd, types.Int, types.Int)
    def minus = Builtin.binOp("-", BinOps.ISub, types.Int, types.Int)
    def times = Builtin.binOp("*", BinOps.IMul, types.Int, types.Int)
    def neq = Builtin.binOp("!=", BinOps.INeq, types.Int, types.Bool)
    def eq = Builtin.binOp("==", BinOps.ICmp, types.Int, types.Bool)
    def lte = Builtin.binOp("<=", BinOps.ILte, types.Int, types.Bool)
    
    val list = List(plus, minus, times, neq, eq, lte)
  }

  val builtinFunDefs = functions.list
  
   
  
  val externFunDefs = List(
      ExternFunDef("printInt", "@printInt", types.Function.create(types.Unit, List(types.Int)), false)
      )

}*/