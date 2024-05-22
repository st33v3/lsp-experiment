package tables

enum ClauseAst[T]:
    case Param(bind: T => Unit)
    case Column(elem: Int, name: String)
    case BinOper(op: String, left: ClauseAst[T], right: ClauseAst[T])
    case UnOper(op: String, operand: ClauseAst[T])
    case Fun(name: String, args: List[ClauseAst[T]])

