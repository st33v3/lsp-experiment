package bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations.Fork
import scala.util.control.NoStackTrace

class ReturnException(val data: Object) extends Exception("ReturnException")

class FastReturnException(val data: Object) extends NoStackTrace

@Fork(1)
class BenchCtrlFlow:

  def normalReturn(level: Int): Object =
    if level == 0 then return new Object()
    else normalReturn(level - 1)

  def exceptionReturn(level: Int): Object =
    if level == 0 then throw new ReturnException(new Object())
    else exceptionReturn(level - 1)

  def fastExceptionReturn(level: Int): Object =
    if level == 0 then throw new FastReturnException(new Object())
    else fastExceptionReturn(level - 1)


  //@Benchmark
  def returnNormally(b: Blackhole): Unit =
    for (j <- 0 until 5) do
      b.consume(normalReturn(5))

  //@Benchmark
  def returnExceptionally(b: Blackhole): Unit =
    for (j <- 0 until 5) do
        try
            exceptionReturn(5)
        catch
            case e: ReturnException => b.consume(e.data)

  @Benchmark
  def returnFastExceptionally(b: Blackhole): Unit =
    for (j <- 0 until 5) do
        try
            fastExceptionReturn(5)
        catch
            case e: FastReturnException => b.consume(e.data)
            