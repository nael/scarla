package nasc

class IdSymbol(val name : String, val definition : SymDef) extends Symbol {
  def isType = false
  def typed = ty != null
}
class TypeSymbol(val name : String, val definition : SymDef) extends Symbol {
  def isType = true
  def typed = definedType != null
} 

object Symbol { var uniq = 0 }

trait Symbol {
  val uniq = { Symbol.uniq += 1; Symbol.uniq}
  override def toString = name + "$" + uniq
  val uniqueName = toString//TODO change this
 
  
  def typed : Boolean
  def name: String
  def definition: SymDef  
  
  def isType : Boolean
  // For typing phase
  var ty : Type = null
  var definedType : Type = null
  
  // For codegen phase
  var storage: SymbolStorage = null 
}