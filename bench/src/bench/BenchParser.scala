package bench


import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole
import incpeg.LineBuffer
import incpeg.BufferSource
import incpeg.RowCol
import incpeg.Traversal
import org.openjdk.jmh.annotations.Fork
import scala.annotation.switch

  class MyParser(val in: Traversal):

    def parsePrimary(): BigInt = 
      if in.current == '(' then
        in.consume()
        val res = parseExpr(1)
        if in.current != ')' then throw new Exception("Expected ')' but '" + in.current.toChar + "' found at " + in.pos.show)
        in.consume()
        return res
      if in.current >= '0' && in.current <= '9' then
        var res = BigInt(0)
        while (in.current >= '0' && in.current <= '9') do 
          res = res * 10 + (in.current - '0')
          in.consume()
        return res
      throw new Exception("Expected digit or '(' at " + in.pos)

    def priority(op: Int): Int = 
      op match
        case '+' => 1
        case '-' => 1
        case '*' => 2
        case _: Int => -1

    def operate(op: Int, a: BigInt, b: BigInt): BigInt = 
      op match
        case '+' => a + b
        case '-' => a - b
        case '*' => a * b
        case _: Int => throw new Exception("Unknown operator at " + in.pos.show)
    
    def parseExpr(prio: Int): BigInt =
      val res = parsePrimary()
      parseExprTail(prio) match
        case null => res
        case (p, v) => operate(p, res, v)

    def parseExprTail(prio: Int): (Int, BigInt) | Null =
      val op = in.current
      val prio2 = priority(op)
      if prio2 < 0 || prio2 < prio then null
      else 
        in.consume()
        val res = parseExpr(prio2)
        if prio2 == prio then (op, res)
        else parseExprTail(prio) match
          case null => (op, res)
          case (op2, v) => (op, operate(op2, res, v))

    def parse(): BigInt =
      val res = parseExpr(1)
      if in.current != Traversal.EOF then throw new Exception("Expected end of input at " + in.pos.show)
      res

object BenchParser:
  
  val lb = LineBuffer.fromString(Texts.expression)
  val bs = BufferSource(lb)

  def parseExprDirect(seq: Sequencer, mult: Boolean): BigInt =
    var res = BigInt(0)
    if seq.current == '(' then
      seq.consume()
      res = parseExprDirect(seq, false)
      if seq.current != ')' then throw new Exception("Expected ')'")
      seq.consume()
    else if seq.current >= '0' && seq.current <= '9' then
      while (seq.current >= '0' && seq.current <= '9') do 
        res = res * 10 + (seq.current - '0')
        seq.consume()
    else throw new Exception("Expected digit or '(' ")
    if seq.current == '*' then
      seq.consume()
      res *= parseExprDirect(seq, true)
    if seq.current == '+' || seq.current == '-' then
      if mult then res
      else 
        val op = seq.current
        seq.consume()
        if op == '+' then res += parseExprDirect(seq, false)
        else res -= parseExprDirect(seq, false)
    res

  @main
  def testParsing(): Unit =
    val in = BenchParser.bs.createTraversal(RowCol(0, 0))
    val mp = MyParser(in)
    println(mp.parse())
    val st = BufferSource(LineBuffer.fromString(Texts.shortExpression)).createTraversal(RowCol(0, 0))
    println(MyParser(st).parse())

@Fork(1)
class BenchParser:

  @Benchmark
  def handmadeParser(b: Blackhole): Unit =
    val in = BenchParser.bs.createTraversal(RowCol(0, 0))
    val mp = MyParser(in)
    val res = mp.parseExpr(1)
    b.consume(res)

  @Benchmark
  def handmadeParserDirect(b: Blackhole): Unit =
    val seq = IntSequencer(Texts.expression)
    val res = BenchParser.parseExprDirect(seq, false)
    b.consume(res)

