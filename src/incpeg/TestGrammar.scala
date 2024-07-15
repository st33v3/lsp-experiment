package incpeg


class TestGrammar extends Grammar:
  type State = EmptyParserState

  val ident = terminal(Recognizer.Ident)
  val number = terminal(Recognizer.Number, _.toInt)
  val expr = nonTerminal[Int]

  
  def declareRules() = 
    expr := number
    expr := (number ~ ident ~ expr).map {case n ~ _ ~ e => n + e}
