package incpeg


abstract class Grammar:
  type State <: ParserState

  protected def createTerminal[T](name: String, recognizer: Recognizer, transform: (String) => T): Terminal[T] = ???
  protected def createNonTerminal[T](name: String): NonTerminal[T] = ???

  inline def terminal(r: Recognizer): Terminal[String] = 
    val name = Named.getName
    createTerminal(name, r, identity)
  
  inline def terminal[T](r: Recognizer, transform: (String) => T): Terminal[T] = 
    val name = Named.getName
    createTerminal[T](name, r, transform)
  
  inline def nonTerminal[T]: NonTerminal[T] = 
    val name = Named.getName
    createNonTerminal[T](name)
  
  def specialize[T](t: Terminal[String], test: (s: String) => T | Null): Terminal[T] = ???
  def optNonTerminal[T](default: T): NonTerminal[T] = ???

  def ruleCount(nt: NonTerminal[?]): Int = ???
  def addRule[T](nt: NonTerminal[T], pos: Int, rule: Rule[T]): Unit = ???

  def rep[T](rule: Rule[T]): Rule[List[T]] = ???
  def rep1[T](rule: Rule[T]): Rule[List[T]] = ???
  def opt[T](rule: Rule[T]): Rule[Option[T]] = ???
  
  def declareRules(): Unit

  extension [T](nt: NonTerminal[T])
    infix def :=(rule: Rule[T]): Unit = ???

  extension [T](rule: Rule[T])
    infix def ~[U](other: Rule[U]): Rule[(T, U)] = ???
    def map[U](f: (T) => U): Rule[U] = ???

  object `~`:
    def unapply[T, U](pair: (T, U)) = pair

