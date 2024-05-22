package incpeg


import utest._

object RowColTest extends TestSuite:
  val tests = Tests:
    test("RowCol"):
      val a = RowCol(1, 2)
      assert(a.row == 1)
      assert(a.col == 2)
      assert(a < RowCol(1, 3))
  
