package incpeg

import utest._

object BufferSourceTest extends TestSuite:

  def extract(traversal: Traversal): String =
    val buf = StringBuilder()
    while (traversal.current != Traversal.EOF) do 
        buf.append(traversal.current.toChar)
        traversal.consume()
    buf.toString()
  
  val tests = Tests:
    test("traverse empty source"):
        val src = BufferSource( LineBuffer.fromString(""))
        assert(src.length == RowCol(0, 0))
        val tr = src.createTraversal(RowCol(0, 0))
        assert(tr.current == Traversal.EOF)
        assert(tr.lookahead == Traversal.EOF)
        assert(tr.lookback == Traversal.EOF)
        assert(extract(tr) == "")

    test("traverse full source"):
        val src = BufferSource( LineBuffer.fromString("line1\nline2"))
        assert(src.length == RowCol(1, 5))
        val tr = src.createTraversal(RowCol(0, 0))
        assert(extract(tr) == "line1\nline2")

    test("traverse part source"):
        val src = BufferSource( LineBuffer.fromString("line1\nline2"))
        assert(src.length == RowCol(1, 5))
        val tr = src.createTraversal(RowCol(1, 1))
        assert(extract(tr) == "ine2")

    test("mark and reset"):
        val src = BufferSource(LineBuffer.fromString("line1\nline2"))
        val tr = src.createTraversal(RowCol(0, 0))
        val m = tr.createMark()
        assert(tr.pos == RowCol(0, 0))
        tr.consume()
        assert(tr.pos == RowCol(0, 1))
        tr.mark(m)
        assert(extract(tr) == "ine1\nline2")
        assert(tr.pos == RowCol(1, 5))
        assert(extract(tr) == "")
        tr.reset(m)
        assert(extract(tr) == "ine1\nline2")