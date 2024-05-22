package tables

enum ClasuseAst[T]:
    case Param(bind: T => Unit)
    case Column(elem: Int, name: String)
    case BinOper(op: String, left: ClasuseAst[T], right: ClasuseAst[T])
    case UnOper(op: String, operand: ClasuseAst[T])
    case Fun(name: String, args: List[ClasuseAst[T]])

