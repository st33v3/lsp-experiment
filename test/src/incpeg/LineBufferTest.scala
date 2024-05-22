package incpeg


import utest._

extension (lb: LineBuffer)
  def extractString(start: RowCol, end: RowCol): String =
    val buf = StringBuilder()
    val len = lb.extract(start, end, buf.underlying)
    assert(buf.length() == len)
    buf.toString()

object LineBufferTest extends TestSuite:
  val tests = Tests:
    test("empty"):
      val lb = LineBuffer()
      assert(lb.isEmpty)
      assert(lb.count == 1)
      assert(lb.length.isZero)

    test("partial line"):
      val lb = LineBuffer(Array("line"))
      assert(!lb.isEmpty)
      assert(lb.length == RowCol(0, 4))
      assert(lb.count == 1)
      lb.extractString(RowCol(0, 0), RowCol(0, 4)) ==> "line"
      lb.extractString(RowCol(0, 1), RowCol(0, 3)) ==> "ine"

    test("full line"):
      val lb = LineBuffer(Array("line", ""))
      assert(!lb.isEmpty)
      assert(lb.length == RowCol(1, 0))
      assert(lb.count == 2)
      assert(lb.extractString(RowCol(0, 0), RowCol(0, 4)) == "line")
      assert(lb.extractString(RowCol(0, 1), RowCol(0, 3)) == "ine")
      assert(lb.extractString(RowCol(0, 1), RowCol(1, 0)) == "ine\n")

