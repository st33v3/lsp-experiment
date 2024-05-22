package incpeg

class CSTNode[+T](
  val pos: RowCol,
  val value: T,
)

trait CSTNodeFactory:
  def terminal[T](input: Input, value: T): CSTNode[T]
  def nonterminal[T](children: List[CSTNode[?]], value: T): CSTNode[T]