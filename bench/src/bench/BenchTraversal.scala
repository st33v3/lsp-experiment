package bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole
import incpeg.LineBuffer
import incpeg.BufferSource
import incpeg.RowCol
import incpeg.Traversal
import org.openjdk.jmh.annotations.Fork

trait Sequencer:
  val seq: CharSequence
  def eof(): Boolean
  def current: Int
  def consume(): Unit

class LongSequencer(val seq: CharSequence) extends Sequencer:
  private var index = 0L
  def eof(): Boolean = index >= seq.length
  def current: Int = if eof() then -1 else seq.charAt(index.toInt)
  def consume(): Unit = 
    index += 1

class IntSequencer(val seq: CharSequence) extends Sequencer:
  private var index = 0
  def eof(): Boolean = index >= seq.length
  def current: Int = if eof() then -1 else seq.charAt(index)
  def consume(): Unit = 
    index += 1

object BenchTraversal:

  val lb = LineBuffer.fromString(Texts.wiki)
  val bs = BufferSource(lb)

@Fork(1)
class BenchTraversal:

  @Benchmark
  def simpleString(b: Blackhole): Unit =
    for (j <- 0 until Texts.wiki.length()) do
      b.consume(Texts.wiki.charAt(j))

  @Benchmark
  def lineBuffer(b: Blackhole): Unit = 
    for (j <- 0 until BenchTraversal.lb.count) do
      for (k <- 0 until BenchTraversal.lb.line(j).length()) do
        b.consume(BenchTraversal.lb.line(j).charAt(k))

  @Benchmark
  def lineBufferTraversal(b: Blackhole): Unit = 
    val tr = BenchTraversal.bs.createTraversal(RowCol(0, 0))
    while (tr.current != Traversal.EOF) do
      b.consume(tr.current)
      tr.consume()

  @Benchmark
  def longSequencer(b: Blackhole): Unit = 
    val seq: Sequencer = new LongSequencer(Texts.wiki)
    while !seq.eof() do
      b.consume(seq.current)
      seq.consume()

  @Benchmark
  def intSequencer(b: Blackhole): Unit = 
    val seq: Sequencer = new IntSequencer(Texts.wiki)
    while !seq.eof() do
      b.consume(seq.current)
      seq.consume()