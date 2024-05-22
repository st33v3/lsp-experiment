package tables

import scala.quoted.*

abstract class SqlParse[T, R]:
    private[tables] def processAst(ast: ClasuseAst[T]): R
    inline def apply(inline condition: T => Boolean): R = ${parseImpl[T, R]('this, 'condition)}


def parseImpl[T: Type, R: Type](self: Expr[SqlParse[T, R]], condition: Expr[T => Boolean])(using Quotes): Expr[R] = 
    import quotes.reflect.*
    val (params, body) = condition.asTerm match
        case Inlined(_, _, Lambda(params, body)) => (params, body)
        case _ => report.errorAndAbort("Expected a lambda expression")
    if params.length != 1 then report.errorAndAbort("Expected a lambda with one parameter")
    // body match
    //     case Apply(_, List(Tuple(args)))
    
    // println("Condition: " + body.show)
    // println("Condition: " + body.show(using Printer.TreeStructure))
    val (stmts, term) = body match
        case Block(stmts, term:  Apply) => (stmts, term)
        case _ => report.errorAndAbort("Expected a block expression")
    val tables = for (s <- stmts) yield
        s match 
            case vdef: ValDef => vdef.name -> vdef.symbol
            case _ => 
                println(s.show(using Printer.TreeStructure))
                report.errorAndAbort("Body of function can only contain parameter untupling and return value", s.pos)
    val tableMap = Map.from(tables)

    def processFunCall(a: Apply) = 
        a.fun match
            case ident: Ident => 
                println(ident.symbol)
            case select: Select => 
                println(select.symbol)
            case _ => report.errorAndAbort("Expected a column reference", a.fun.pos)

    processFunCall(term)
    '{$self.processAst(ClasuseAst.Fun("filter", List(ClasuseAst.Column(0, "a"), ClasuseAst.Column(0, "b"))))}
