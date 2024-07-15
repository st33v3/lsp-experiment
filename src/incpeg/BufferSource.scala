package incpeg

case class BufferMark(var row: Int, var col: Int, var lb: Int)

class BufferTraversal(private val buffer: LineBuffer, private var row: Int, private var col: Int) extends Traversal:

    type Mark = BufferMark

    private var _lookback: Int = Traversal.EOF
    private var _current: Int = Traversal.EOF
    private var _lookahead: Int = Traversal.EOF
    private var laRow: Int = row
    private var laCol: Int = col
    private var line: String = ""

    assert(row < buffer.count && col <= buffer.line(row).length())
    line = buffer.line(row)
    _current = get()
    next()
    _lookahead = get()

    def createMark(): Mark = BufferMark(-1, -1, -1)

    def mark(mark: Mark): Unit = 
        mark.row = row
        mark.col = col
        mark.lb = _lookback
    
    def reset(mark: Mark): Unit = 
        col = mark.col
        row = mark.row
        laCol = col
        laRow = row
        line = buffer.line(row)
        _lookback = mark.lb
        _current = get()
        next()
        _lookahead = get()

    private inline def next(): Unit =
        laRow = row
        laCol = col + 1
        if laCol > line.length() then
            if (laRow < buffer.count - 1) then
                laRow += 1
                laCol = 0
                line = buffer.line(laRow)

    private inline def get(): Int = 
        val ll = line.length()
        if laCol < ll then line.charAt(laCol)
        else if laCol == ll then '\n'
        else Traversal.EOF

    def current: Int = _current
    
    def lookahead: Int = _lookahead

    def consume(): Unit = 
        if _current == Traversal.EOF then throw new IllegalStateException("Cannot consume EOF")
        _lookback = current
        _current = lookahead
        row = laRow
        col = laCol
        next()
        _lookahead = get()
            
    def pos = RowCol(row, col)
    def lookback: Int = _lookback

class BufferSource(private val buffer: LineBuffer) extends Source:
    def length: RowCol = buffer.length
    def charAt(pos: RowCol): Char = buffer.charAt(pos)
    def createTraversal(pos: RowCol): Traversal = new BufferTraversal(buffer, pos.row, pos.col)
    def extract(pos: RowCol, count: RowCol, sink: Appendable): Int = buffer.extract(pos, count, sink)
    def lineLength(row: Int): Int = buffer.line(row).length
