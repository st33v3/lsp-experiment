package incpeg

case class BufferMark(var row: Int, var col: Int, var lb: Int)

class BufferTraversal(private val buffer: LineBuffer, private var row: Int, var col: Int) extends Traversal:

    type Mark = BufferMark

    private var _lookback: Int = -1
    assert(row < buffer.count && col <= buffer.line(row).length())
    
    def createMark(): Mark = BufferMark(-1, -1, -1)

    def mark(mark: Mark): Unit = 
        mark.row = row
        mark.col = col
        mark.lb = _lookback
    
    def reset(mark: Mark): Unit = 
        col = mark.col
        row = mark.row
        _lookback = mark.lb

    protected def get(r: Int, c: Int): Int = 
        val line = buffer.line(r)
        if c == line.length() then 
            if r == buffer.count - 1 then Traversal.EOF else '\n'
        else line.charAt(c)

    def current: Int = get(row, col)
    
    def lookahead: Int = 
        if col < buffer.line(row).length then get(row, col + 1)
        else 
            if row < buffer.count - 1 then get(row + 1, 0) else Traversal.EOF

    def consume(): Unit = 
        _lookback = current
        if col < buffer.line(row).length() then col += 1
        else
            col = 0
            if row < buffer.count - 1 then row += 1
            


    def pos = RowCol(row, col)
    def lookback: Int = _lookback

class BufferSource(private val buffer: LineBuffer) extends Source:
    def length: RowCol = buffer.length
    def charAt(pos: RowCol): Char = buffer.charAt(pos)
    def createTraversal(pos: RowCol): Traversal = new BufferTraversal(buffer, pos.row, pos.col)
    def extract(pos: RowCol, count: RowCol, sink: Appendable): Int = buffer.extract(pos, count, sink)
    def lineLength(row: Int): Int = buffer.line(row).length
