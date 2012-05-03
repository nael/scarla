package nasc
object Type {

}

trait Type {
  def name: String
  def llvmType: String
  def typeSymbol: TypeSymbol
  def concreteType: ConcreteType
  def memberType(m: String): Option[Type]
  def hasMember(m: String) = memberType(m) != None
  def memberSymbol(m: String): Option[Symbol]
  def bareType: BareType

  def attributes: Types.Attributes.ValueSet
  
  def isRef = attributes.contains(Types.Attributes.Ref)

  override def toString = attributes.map(_.toString() + " ").toList.mkString(" ") + typeSymbol.toString()
}

trait BareType extends Type {
  def bareType: BareType = this
}

trait ConcreteType extends Type {
  def concreteType: ConcreteType = this
}

trait SimpleType extends ConcreteType with BareType {
  def memberType(m: String) = None
  def memberSymbol(m: String) = None
  def attributes = Types.Attributes.ValueSet()
}

trait TypeFunctor extends ConcreteType with BareType {
  def llvmType = Utils.error("Cannot compile functors")
  def instanciate(symbol: TypeSymbol, args: List[Type]): Type
  def instanciate(args: List[Type]): Type
}

trait SimpleTypeFunctor extends TypeFunctor {

  private var instances = Map[List[Type], Type]()

  def memberType(m: String) = None
  def memberSymbol(m: String) = None
  def attributes = Types.Attributes.ValueSet()
  def instanciate(arguments: List[Type]) = {
    val args = arguments.map(_.concreteType)
    instances.get(args) match {
      case None => {
        val sym = new TypeSymbol(name + "[" + Utils.repsep(arguments.map(_.toString)) + "]", typeSymbol.definition)
        val res = instanciate(sym, arguments) //TODO change this?
        sym.definedType = res
        instances += args -> res
        res
      }
      case Some(inst) => inst
    }

  }
}

object Types {

  object Attributes extends Enumeration {
    type Attr = Value
    val Readonly, Ref = Value
  }

  object Aggregate {
    trait Element {
      def name: String
      def symbol: Symbol
    }
    case class Field(override val name: String, override val symbol: Symbol) extends Element
    case class Function(override val name: String, override val symbol: Symbol) extends Element
  }

  class Trait(
    val typeSymbol: TypeSymbol,
    val fields: List[Aggregate.Element]) extends ConcreteType with BareType {
    
    val name = "trait " + typeSymbol
    def llvmType = "i1"
    def memberField(m: String) = fields.find(_.name == m)
    def symbolMember(s : Symbol) = fields.find(_.symbol == s)
    def memberType(m: String) = memberField(m).map(_.symbol.ty)
    def memberSymbol(m: String) = memberField(m).map(_.symbol)
    def attributes = Attributes.ValueSet()
    
    def findMember(f : Aggregate.Element => Boolean) : Option[Aggregate.Element] =
      fields.find(f)//.orElse(traits.foldLeft(None : Option[Aggregate.Element]){case(u, t) => u.orElse(t.findMember(f))})
    
  }
  
  object ClassMods extends Enumeration {
    type Mod = Value
    val Val, Case = Value
  }
  
  class Struct(
    val typeSymbol: TypeSymbol,
    val traits: List[Trait],
    val fields: List[Aggregate.Element],
    val modifiers: ClassMods.ValueSet = ClassMods.ValueSet()) extends ConcreteType with BareType {
	  
    
    val name = "struct " + typeSymbol
    def initSymbol = typeSymbol.definition.asInstanceOf[StructDefinition].initSymbol
    def llvmType = "{" + Utils.repsep(fields.filter { case _: Aggregate.Field => true case _ => false }.map(_.symbol.ty.llvmType)) + "}" + (if(isValueClass) "" else "*")
    def findMember(f : Aggregate.Element => Boolean) : Option[Aggregate.Element] =
      fields.find(f).orElse(traits.foldLeft(None : Option[Aggregate.Element]){case(u, t) => u.orElse(t.findMember(f))})
    def memberField(m: String) =
      findMember(_.name == m)
    def memberType(m: String) = memberField(m).map(_.symbol.ty)
    def memberSymbol(m: String) = memberField(m).map(_.symbol)
    def symbolMember(s : Symbol) = findMember(_.symbol == s)
    def attributes = Attributes.ValueSet()
    def isValueClass = modifiers.contains(ClassMods.Val)
  }

  class Named(val typeSymbol: TypeSymbol) extends Type with BareType {
    val name = typeSymbol.name
    //TODO move this somewhere else
    def llvmType = {
     if (concreteType.typeSymbol == Defs.types.Unit.typeSymbol)
       concreteType.llvmType
     else if(typeSymbol.storage != null) {  typeSymbol.storage.asRaw.name }
     else concreteType.llvmType
    }
    override def concreteType = typeSymbol.definedType.concreteType
    def memberType(m: String) = concreteType.memberType(m)
    def memberSymbol(m: String) = concreteType.memberSymbol(m)
    def attributes = typeSymbol.definedType.attributes
  }
 
  object Qual {
    def removeAttribute(t : Type, attr : Types.Attributes.Attr) = {
      new Qual(t.bareType, t.attributes - attr)
    }
    def addAttribute(t : Type, attr : Types.Attributes.Attr) = {
      new Qual(t.bareType, t.attributes + attr)
    }
  }
  
  class Qual(val underlying: Type, val attributes: Attributes.ValueSet) extends Type with ConcreteType {

    def this(underlying: Type, attr: Attributes.Value) = {
      this(underlying, Attributes.ValueSet(attr))
    }

    val name = underlying.name
    def llvmType = underlying.llvmType + (if(attributes.contains(Attributes.Ref)) "*" else "")
    def memberType(m: String) = underlying.memberType(m).map(new Qual(_, attributes))
    def memberSymbol(m: String) = underlying.memberSymbol(m)
    def typeSymbol = underlying.typeSymbol
    def bareType = underlying.bareType
  }

}