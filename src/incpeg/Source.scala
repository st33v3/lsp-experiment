package incpeg

trait Source {
  def length: RowCol
  def charAt(pos: RowCol): Char
  def createTraversal(pos: RowCol): Traversal
  def extract(pos: RowCol, count: RowCol, sink: Appendable): Int
  def lineLength(row: Int): Int
}
