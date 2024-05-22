package incpeg

trait Traversal:
    type Mark
    def createMark():Mark
    def pos: RowCol
    def mark(mark: Mark): Unit
    def reset(mark: Mark): Unit
    def current: Int
    def lookahead: Int
    /**
      * Consumes the next character in the input. Consuming EOF results in an error.
      */
    def consume(): Unit
    def lookback: Int

object Traversal:
    val EOF = -1
    
trait Input:
    protected val traversal: Traversal
    export traversal.*
    def start: RowCol
    /**
      * Character before start position (may be EOF for first character in input)
      */
    def lookbackTerminal: CSTNode[?] | Null

