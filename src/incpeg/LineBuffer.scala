package incpeg

import javax.sound.sampled.Line


class LineBuffer(
    private var lines: Array[String],
    private var index: Int,
    private var size: Int):
    assert(index >= 0 && size > 0 && index < size && size <= lines.length, s"index: $index, size: $size, lines.length: ${lines.length}")

    def this() = this(Array(""), 0, 1)
    def this(lines: Array[String]) = this(lines, 0, lines.length)

    def count = size - index

    protected def remaining: Int = lines.length - size

    protected def enlarge(newSize: Int): Unit = 
        if newSize <= lines.length then return
        val newLines = Array.ofDim[String](((newSize - 1) / 16 + 1) * 16)
        Array.copy(lines, 0, newLines, 0, size)
        lines = newLines
        size = newSize

    def isEmpty: Boolean = count == 1 && line(0).isEmpty

    def line(i: Int): String = 
        assert(i<count)
        lines(index + i)    

    def relocate(): Unit = 
        for (i <- 0 until index) do lines(i) = lines(i + index)
        size -= index
        index = 0

    def append(buf: LineBuffer, offset: RowCol = RowCol.zero):Unit =
        if (buf.count == 0) then return
        assert(buf.count > offset.row && offset.col < buf.lines(buf.index + offset.row).length)
        if count == 0 then 
            enlarge(buf.size - offset.row)
            index = 0
            size = buf.size - offset.row
            Array.copy(buf.lines, buf.index + offset.row, lines, 0, size)
            lines(offset.row) = buf.lines(offset.row).substring(offset.col).nn
        else 
            if remaining <= buf.count then
                relocate()
                enlarge(size + buf.count)
            val last = index + size - 1
            lines(last) = lines(last) + buf.lines(buf.index + offset.row).substring(offset.col)
            Array.copy(buf.lines, buf.index + offset.row + 1, lines, index + size, buf.count - offset.row - 1)
            size += buf.count - offset.row - 1

    def extract(offset: RowCol, count: RowCol, sink: Appendable): Int =
        if count.isZero then return 0
        var start = index + offset.row
        assert(start < size && start + count.row <= size && lines(start).length >= offset.col)
        var line = lines(start).substring(offset.col).nn
        var rowCount = count.row
        var ret = 0
        while rowCount > 0 do
            sink.append(line)
            sink.append('\n')
            ret += line.length() + 1
            rowCount -= 1
            start += 1
            line = lines(start)
        if count.col > 0 then  
            assert(line.length() <= count.col)
            sink.append(line.substring(0, count.col).nn)
            ret += count.col
        ret

    def charAt(pos: RowCol): Char = 
        val str = line(pos.row)
        assert(pos.col < str.length)
        str.charAt(pos.col)
    
    def length: RowCol = RowCol(count - 1, lines(count - 1).length)

object LineBuffer:
    def fromString(str: String): LineBuffer = 
        if (str.isEmpty) return LineBuffer()
        val lines = str.split('\n')
        LineBuffer(lines)