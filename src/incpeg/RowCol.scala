package incpeg

import scala.language.unsafeNulls

opaque type RowCol = Long

object RowCol:
  
  def apply(row: Int, col: Int): RowCol = 
    assert(row >= 0)
    assert(col >= 0)
    row.toLong << 32 | col.toLong

  def zero: RowCol = 0L
  def invalid: RowCol = -1L

  // Following code does not work in combination with explicit null
  // given Ordering[RowCol] with
  //    def compare(x: RowCol, y: RowCol): Int = java.lang.Long.compare(x.nn, y.nn)

extension (pos: RowCol)
    def row: Int = (pos >> 32).toInt
    def col: Int = pos.toInt & 0x7FFFFFFF
    def isZero: Boolean = pos == 0
    def +(n: RowCol): RowCol = 
      if n.row == 0 then RowCol(pos.row, n.col + pos.col)
      else RowCol(pos.row + n.row, n.col)
    def -(n: RowCol): RowCol =
      if pos.row == n.row then RowCol(pos.row, pos.col - n.col)
      else RowCol(pos.row - n.row, pos.col)
    def <(other: RowCol): Boolean = pos < other
    def <=(other: RowCol): Boolean = pos <= other
    def >(other: RowCol): Boolean = pos > other
    def >=(other: RowCol): Boolean = pos >= other



