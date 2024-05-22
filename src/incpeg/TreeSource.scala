package incpeg
import scala.collection.mutable.ListBuffer

val MAX_WEIGHT = 1024
val LINE_WEIGHT = 16
val DEGREE = 10

def newChildren(): Array[LineTreeNode] = Array.ofDim[LineTreeNode](DEGREE * 2 + 1)


 trait LineTreeNode {
  def length: RowCol
  def charAt(pos: RowCol): Char
  def createTraversal(pos: RowCol): Traversal
  def extract(pos: RowCol, count: RowCol, sink: Appendable): Int
  def edit(change: Edit): Unit
  def isUnderflow: Boolean
  def isOverflow: Boolean
  /**
    * Retrieves length of a portion of given line that belongs to the node
    *
    * @param line number of line
    * @return length of the line in characters (not including line terminator)
    */
  def lineLength(line: Int): Int = ???
}

class TreeSource extends Source:
    private var root: LineTreeInner | Null = null
    protected def strictRoot: LineTreeInner = 
        val r = root
        if r != null then r else throw new IllegalStateException("Tree is empty")

    def length: RowCol = if root == null then RowCol.zero else strictRoot.length
    def charAt(pos: RowCol): Char = strictRoot.charAt(pos)
    def createTraversal(pos: RowCol): Traversal = strictRoot.createTraversal(pos)

    def edit(change: Edit): Unit =
        if root == null then
            assert(change.delete.isZero && change.pos.isZero)
            val leaf = new LineTreeLeaf()
            val children = newChildren()
            children(0) = leaf
            root = new LineTreeInner(children, 1)
        while (!change.isDone) do strictRoot.edit(change)

    def extract(pos: RowCol, count: RowCol, sink: Appendable): Int = if root == null then 0 else strictRoot.extract(pos, count, sink)
    
    def lineLength(line: Int): Int = ???


class Edit(var pos: RowCol, var delete: RowCol, var buffer: LineBuffer):
    def isDone: Boolean = delete.isZero && buffer.isEmpty
    def append(buf: LineBuffer, offset: RowCol = RowCol.zero): Unit = buffer.append(buf, offset)


class LineTreeInner(private var children: Array[LineTreeNode], private var size: Int) extends LineTreeNode:
    private var _length: RowCol = RowCol.invalid
    assert(children.length >= size)

    override def createTraversal(pos: RowCol): Traversal = ???

    override def charAt(pos: RowCol): Char = 
        val i = 0
        var pos0 = pos
        while (i < size) do
            val child = children(i)
            val len = child.length
            if pos0 < len then return child.charAt(pos0)
            pos0 = pos0 - len
        throw new IllegalArgumentException("Position out of bounds: " + pos + " in " + length)

    override def edit(data: Edit): Unit = 
        ???
        _length = RowCol.invalid

    override def length: RowCol = 
        if _length == RowCol.invalid then
            _length = RowCol.zero
            for i <- 0 until size do _length += children(i).length
        _length

    override def extract(pos: RowCol, count: RowCol, sink: Appendable): Int = ???

    private def insert(node: LineTreeNode, pos: Int): Unit = 
        assert(!isOverflow)
        System.arraycopy(children, pos, children, pos + 1, size - pos)
        children(pos) = node
        size += 1

    def isOverflow: Boolean = size > DEGREE * 2
    def isUnderflow: Boolean = size < DEGREE

class LineTreeLeaf extends LineTreeNode:
    private val lines: LineBuffer = LineBuffer()    
    private var _weight: Int = -1

    def weight: Int =
        if _weight == -1 then
            _weight = 0
            for i <- 0 until lines.count do _weight += lines.line(i).length()
            if lines.count != 0 then _weight += (lines.count - 1) * LINE_WEIGHT
        _weight

    override def isOverflow: Boolean = weight > MAX_WEIGHT

    override def createTraversal(pos: RowCol): Traversal = ???

    override def charAt(pos: RowCol): Char = lines.charAt(pos)

    override def edit(data: Edit): Unit = 
        ???
        _weight = -1

    override def length: RowCol = ???

    override def isUnderflow: Boolean = ???

    override def extract(pos: RowCol, count: RowCol, sink: Appendable): Int = ???


    protected def delete(pos: RowCol, delete: RowCol): Unit =
        // if delete.isZero then RowCol(0, 0)
        // if delete.row == 0 then
        //     val line = lines(pos.row)
        //     val before = line.substring(0, pos.col)
        //     if pos.col + delete.col > lines(pos.row).length then
        //         if (pos.row != size - 1) then throw new IllegalArgumentException("Delete beyond end of line")
        //         /* othrewise deletion may continue at following node*/
        //     val after = lines(pos.row + 1)
        //     lines(pos.row) = before + after
        //     RowCol(0, 0) //TODO
        // else
        //     //delete remainder of the line
        //     lines(pos.row) = lines(pos.row).substring(0, pos.col)
        //     if pos.row + 1 + delete.row >= size then
        //         ()
        //         //Deletion goes beyond this node
        //         //size = pos.row + 1
        //     else

        //         val newLines = Array.ofDim[String](size - delete.row)
                //Array.copy(lines, 0, newLines, 0, pos.row)
                //Array.copy(lines, pos.row + delete.row, newLines, pos.row, size - pos.row - delete.row)
                //lines = newLines

            // lines = Array.ofDim[String](pos.row)
            // val before = lines(pos.row).substring(0, pos.col)
            // val after = lines(pos.row + data.delete.row).substring(data.pos.col + data.delete.col)
            // lines(data.pos.row) = before + after
            // val newLines = Array.ofDim[String](lines.length - data.delete.row)
            // Array.copy(lines, 0, newLines, 0, data.pos.row)
            // Array.copy(lines, data.pos.row + data.delete.row, newLines, data.pos.row, lines.length - data.pos.row - data.delete.row)
            // lines = newLines
            // length = Pos(length.row - data.delete.row, length.col - data.delete.col)

            ()