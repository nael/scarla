package nasc

class IdSymbol(val name : String, var definition : SymDef) extends Symbol {
  def isType = false
  def typed = ty != null
  var ty : Type = null
}
class TypeSymbol(val name : String, var definition : SymDef) extends Symbol {
  def isType = true
  def typed = definedType != null
  var definedType : Type = null
  
  def replaceBy(ts: TypeSymbol) = {
    uniqueName = ts.uniqueName
    definedType = ts.definedType
    definition = ts.definition
    storage = ts.storage
  }
} 

object Symbol { var uniq = 0 }

trait Symbol {
  val uniq = { Symbol.uniq += 1; Symbol.uniq}
  override def toString = name + "$" + uniq
  var uniqueName = toString//TODO change this
 
  
  def typed : Boolean
  def name: String
  def definition: SymDef
  
  def isType : Boolean
  // For typing phase
  
  
  
  // For codegen phase
  var storage: SymbolStorage = null 
}