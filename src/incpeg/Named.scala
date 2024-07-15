package incpeg

import scala.quoted.*

object Named:
  inline def getName: String = ${getNameMacro}
  def getNameMacro(using Quotes): Expr[String] = 
    import quotes.*
    import reflect.*
    var ctx = Symbol.spliceOwner.owner.owner
    val owner = ctx.owner
    if (!owner.isClassDef || !ctx.isValDef) report.errorAndAbort("Value of non-terminal must be assigned to a val in a class")
    Expr(ctx.name)


    
