package object bench
  
import scala.util.Random

object ArithmeticExpressionGenerator:
  val operators = List('+', '-', '*')
  val random = new Random()

  def generateExpression(length: Int): String =
    val sb = new StringBuilder
    var openParens = 0
    var allowClose = false
    var allowOpen = true
    var allowOper = false
    var allowNum = true

    while sb.length() < length do
      if allowClose && random.nextInt(10) < openParens then
        sb.append(')')
        openParens -= 1
        allowOpen = false
        allowNum = false
        allowOper = true
      if allowOpen && random.nextBoolean() then
        sb.append('(')
        openParens += 1
        allowOper = false
        allowClose = false
        allowNum = true
      if allowNum && random.nextBoolean() then
        sb.append(random.nextInt(1000))
        allowOpen = false
        allowClose = true
        allowOper = true
        allowNum = false
      if allowOper && random.nextBoolean() then
        sb.append(operators(random.nextInt(operators.length)))
        allowOpen = true
        allowNum = true
        allowOper = false
        allowClose = false

    if !allowClose && openParens > 0 then
      sb.append('1')
    while openParens > 0 do
      sb.append(')')
      openParens -= 1


    sb.toString()


  @main
  def runGenerator(): Unit =
      println(ArithmeticExpressionGenerator.generateExpression(10240))
