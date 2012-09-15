package nasc

class VirtualPhase extends Phase[Tree, Tree] {

  def name = "virtual"

  var traitVTables = Map[Symbol, Symbol]()
  var defToVtPtr = Map[DefDef, Symbol]()

  // the traits typeInfo will be erased by buildVTable as we replace them by concrete structs and type them
  // so we keep it here for the 2nd phase
  var traitTypeInfos = Map[Symbol, TypeInfo]()

  // upcasts(A,B) is the symbol of the globally defined vtable for interface B on concrete type A
  var upcasts = Map[(Symbol, Symbol), Symbol]()
  
  // links the symbol of a method to an eventual default implementation provided by the trait definition
  var defaultImplemetations = Map[Symbol, Symbol]()
  
  // map trait sym to their mixedIn traits sym for convenience
  var traitUpcasts = Map[Symbol,Seq[Symbol]]()

  def buildVTable(td: TypeDef, t: Trait): Tree = {
    val ty = td.typeName.symbol
    val funDefs = ty.typeInfo.members map { _.definition } collect { case dd: DefDef => dd }

    traitTypeInfos += ty -> ty.typeInfo

    val vtPtrs = funDefs map { dd =>
      val dps = new Symbol {
        val name = dd.defName.symbol.name + "_ptr"
        var typeSymbol: Symbol = null
        var isType = false
        var definition: Def = null
      }
      val ft = dd.defName.symbol.typeSymbol
      val n = Builtin.functionArgTypes(ft).size
      val nft = new Apply(new Sym(Builtin.Functions(n + 1).symbol), new Sym(ty) +: (Builtin.functionArgTypes(ft) map { new Sym(_) }) :+ new Sym(Builtin.functionReturnType(ft)))
      val vd = new ArgDef(new Sym(dps), nft)
      defToVtPtr += dd -> dps
      dps.definition = vd
      vd.attr += attributes.Val()
      vd
    }
    
    // If trait provides a default implementations
    val defaultImpls = funDefs flatMap { dd =>
      dd.body map { default =>
        val defaultSym = new Symbol {
          val name = dd.defName.symbol.name + "_default"
          var typeSymbol: Symbol = null
          var isType = false
          var definition: Def = null
        }
        defaultImplemetations += dd.defName.symbol -> defaultSym
        val ddd = new DefDef(new Sym(defaultSym), dd.arguments, dd.returnTypeTree, dd.body)
        defaultSym.definition = ddd
        ddd
      }
    }
    
    // Pointers to vtable of parent traits
    val upcastPointers = t.composedTraits map { c =>
      val destSym = c.symbol
      val upPtrSym = new Symbol {
        val name = destSym.name + "_vt"
        var typeSymbol: Symbol = null
        var isType = false
        var definition: Def = null
      }
      val vd = new ArgDef(new Sym(upPtrSym), new Sym(traitVTables(destSym)))
      upPtrSym.definition = vd
      vd.attr += attributes.Val()
      traitUpcasts += ty -> ((traitUpcasts get ty getOrElse (Seq())) :+ destSym)
      upcasts += (ty, destSym) -> upPtrSym
      vd
    }
    
    val vtSym = new Symbol {
      def name = ty + "_vt"
      var typeSymbol: Symbol = null
      var isType = true
      var definition: Def = null
    }
    val st = new Struct(vtPtrs ++ upcastPointers, Seq(), new Block(Seq()))
    val vtDef = new TypeDef(new Sym(vtSym), Seq(), Some(st))
    vtDef.attr += attributes.Move()
    vtSym.definition = vtDef

    td.typeSymbol = null
    val objPtrSym = new Symbol {
      def name = "ptr"
      var typeSymbol: Symbol = null
      var isType = false
      var definition: Def = null
    }
    val objPtrDef = new ArgDef(new Sym(objPtrSym), new Sym(Builtin.CPtr.symbol))
    objPtrDef.attr += attributes.Val()
    objPtrSym.definition = objPtrDef

    val vtPtrSym = new Symbol {
      def name = "vt"
      var typeSymbol: Symbol = null
      var isType = false
      var definition: Def = null
    }
    val vtPtrDef = new ArgDef(new Sym(vtPtrSym), new Sym(vtSym))
    vtPtrDef.attr += attributes.Val()
    vtPtrSym.definition = vtPtrDef

    val concrete = new Struct(Seq(), Seq(), new Block(Seq()))

    funDefs foreach { dd =>
      val s = new Select(new Sym(vtPtrSym), new Sym(defToVtPtr(dd)))
      val a = new Apply(s, concrete.thisTree +: (dd.arguments map { arg => new Sym(arg.argName.symbol) }))
      dd.body = Some(a)
      dd.typeSymbol = null
      //dd.defName.typeSymbol = null
    }

    concrete.arguments = Seq(
      objPtrDef,
      vtPtrDef
    )
    concrete.content = new Block(funDefs ++ defaultImpls)

    td.value = Some(concrete)
    td.attr -= attributes.Move()
    td.attr += attributes.CopyThis()

    traitVTables += ty -> vtSym

    Typer.typeTree(new Block(Seq(td, vtDef)))
  }

  val buildVTables: PartialFunction[Tree, Tree] = {
    case td: TypeDef => {
      td.value match {
        case Some(t: Trait) => {
          buildVTable(td, t)
        }
        case _ => td
      }
    }
  }
  
  def tableSymbol(concrete: Symbol, tr: Symbol) = {
    val t = upcasts.getOrElse((concrete,tr), new Symbol {
        def name = concrete.uniqueName + "_to_" + tr.uniqueName + "_vt"
        var typeSymbol: Symbol = traitVTables(tr)
        var isType = false
        var definition: Def = null
      })
    upcasts += (concrete, tr) -> t
    t
  }

  def implementTable(td: TypeDef, s: Struct): Tree = {
    var tables = Seq[Tree]()
    s.composedTraits foreach { t =>
      val traitSym = t.symbol
      val ty = td.typeName.symbol
      
      val tableType = traitVTables(traitSym)
      val tableSym = tableSymbol(ty, traitSym)

      // Now generate stub methods
      val corres = traitTypeInfos(traitSym).members map { member =>
        (ty.typeInfo.members.find { _.name == member.name } map // TODO in typing phase add a sym1.implements(sym2) or sthing
          { concMember =>
            concMember -> concMember//defToVtPtr(member.definition.asInstanceOf[DefDef])
          }) getOrElse {
            val fun = ty.typeInfo.derived get member getOrElse { Utils.error("Not implemented : " + member.name + " in " + ty) }
            fun -> (defaultImplemetations get fun getOrElse { Utils.error("No default impl and no implemented : " + fun + ". Should have failed typing earlier.\n" + defaultImplemetations + "\n" + ty.typeInfo.derived) })
          }
      }
      
      if(G.verbose) println("Override table : " + corres)
      val stubs = (corres map {
        case (fun, fptr) => {
          val stubSym = new Symbol {
            def name = fun.uniqueName + "_stub"
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = null
          }
          var i = 0
          val args = Builtin.functionArgTypes(fun.typeSymbol) map { argType =>
            val argSym = new Symbol {
              val name = "arg_" + i
              var typeSymbol: Symbol = null
              var isType = false
              var definition: Def = null
            }
            i += 1
            val ad = new ArgDef(new Sym(argSym), new Sym(argType))
            argSym.definition = ad
            ad
          }
          val argSyms = args map { _.argName.symbol }
          val thisSym = new Symbol {
            def name = "this"
            var typeSymbol: Symbol = null
            var isType = false
            var definition: Def = null
          }
          val thisArg = new ArgDef(new Sym(thisSym), new Sym(traitSym))
          thisSym.definition = thisArg

          val concreteThis = new cast.BitCast(new Select(new Sym(thisSym), new Name("ptr", false)), new Sym(s.thisTree.typeSymbol))
          val stubBody = new Apply(new Select(concreteThis, new Sym(fptr)), argSyms map { new Sym(_) })

          val dd = new DefDef(new Sym(stubSym), thisArg +: args, new Sym(Builtin.functionReturnType(fun.typeSymbol)), Some(stubBody))
          stubSym.definition = dd
          tables +:= dd
          stubSym
        }
      }) ++ (traitUpcasts getOrElse(traitSym, Seq()) map { tableSymbol(ty, _)})
      val table = new New(new Sym(tableType), stubs map { new Sym(_) })
      val vdd = new ValDef(new Sym(tableSym), new Sym(tableType), Some(table))

      tableSym.definition = vdd
      tables +:= vdd
    }
    Typer.typeTree(new Block(td +: tables))
  }

  val implementTables: PartialFunction[Tree, Tree] = {
    case td: TypeDef => {
      td.value match {
        case Some(s: Struct) => implementTable(td, s)
        case _ => td
      }
    }
  }

  val explicitUpCasts: PartialFunction[Tree, Tree] = {
    case uc: cast.UpCast => {
      val vt = if(traitUpcasts.getOrElse(uc.value.typeSymbol, Seq()).contains(uc.typeSymbol)) { // We are casting a trait to a trait
        new Select(new Select(uc.value, new Name("vt", false)), new Sym(upcasts(uc.value.typeSymbol, uc.typeSymbol)))
      } else new Sym(upcasts(uc.value.typeSymbol, uc.typeSymbol)) // We are casting a concrete type to a trait
      Typer.typeTree(new New(new Sym(uc.typeSymbol), Seq(new cast.BitCast(uc.value, new Sym(Builtin.CPtr.symbol)), vt)))
    }
  }

  def execute(t: Tree): Tree = {
    val res = ((t transform buildVTables) transform implementTables) transform explicitUpCasts

    Typer.check(res) // TODO can remove normally

    res
  }

}