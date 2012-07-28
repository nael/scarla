package nasc

object Symbol {
  var uniq = 0
}

trait Symbol {
  val uniq = { Symbol.uniq += 1; Symbol.uniq }
  override def toString = name + "$" + uniq
  val uniqueName = toString //TODO change this

  def typed = (!isType) && typeSymbol != null
  def name: String
  var definition: Def

  var isType: Boolean
  var typeVars: Iterable[(Symbol, Symbol)] = List()
  var derivedSymbols: List[Symbol] = List()
  
  // For typing phase
  var typeSymbol: Symbol

  // For codegen phase
  var storage: SymbolStorage = null 
}