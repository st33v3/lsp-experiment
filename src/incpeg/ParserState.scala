package incpeg

abstract class ParserState:
    def count: Int
    def reset(): Unit
    def accessMask: Int
    def changeMask: Int
    def equalWithMask(other: ParserState, mask: Int): Boolean
    def copy(): ParserState

class EmptyParserState extends ParserState:
    def count: Int = 0
    def reset(): Unit = ()
    def accessMask: Int = 0
    def changeMask: Int = 0
    def equalWithMask(other: ParserState, mask: Int): Boolean = other.isInstanceOf[EmptyParserState]
    def copy(): ParserState = this