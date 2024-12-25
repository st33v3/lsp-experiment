package incpeg

trait Source {
  def length: RowCol
  /**
    * Retrieves character at given position. Not that last character of a line
    * is newline character, if there is next line after line provided.
    *
    * @param pos position of character
    * @return character at given position
    * @throws Exception if position is out of bounds
    */
  def charAt(pos: RowCol): Char
  /**
    * Retrieves position of next character after given position. Returns `invalid`
    * if there is no character after given position.
    *
    * @param pos current position
    * @return position of next character
    */
  def nextPos(pos: RowCol): RowCol
  def createTraversal(pos: RowCol): Traversal
  def extract(pos: RowCol, count: RowCol, sink: Appendable): Int
  def lineLength(row: Int): Int
}
