import org.parboiled2.*

class Calculator(val input: ParserInput) extends Parser:
  def InputLine = rule: 
    Expression ~ EOI

  def Expression: Rule1[Int] = rule:
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
    | '-' ~ Term ~> ((_: Int) - _)
    )
  
  def Term:Rule1[Int] = rule:
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
    | '/' ~ Factor ~> ((_: Int) / _))

  def Factor = rule: 
    Number | Parens 

  def Parens = rule: 
    '(' ~ Expression ~ ')' 

  def Number = rule: 
    capture(Digits) ~> (_.toInt)

  def Digits = rule: 
    oneOrMore(CharPredicate.Digit)

object ParserTest:
    @main
    def main(): Unit =
        val result = new Calculator("1+1").InputLine.run()
        println(result)