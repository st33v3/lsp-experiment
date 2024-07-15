package incpeg

trait Rule[T]

abstract class Symbol[T](val ordinal: Int):
    def kind: String
    def name: String

abstract class Terminal[T](ordinal: Int) extends Symbol[T](ordinal) with Rule[T]:
    def kind = "Terminal"

abstract class NonTerminal[T](ordinal: Int) extends Symbol[T](ordinal) with Rule[T]:
    def kind = "NonTerminal"


