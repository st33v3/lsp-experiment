
abstract class Symbol[+T](val ordinal: Int):
    def name: String
    
class Terminal[+T](ordinal: Int) extends Symbol[T](ordinal):
    def name = "Terminal"

class NonTerminal[+T](ordinal: Int) extends Symbol[T](ordinal):
    def name = "NonTerminal"


