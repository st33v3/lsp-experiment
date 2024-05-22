package tables

import utest._
import sqlite.Binder
import sqlite.Extractor
import tables.PrimaryKey
import tables.Index
import tables.tabulate
import zio.json.JsonCodec

case class Address(
    street: String,
    city: String,
    zip: String
) derives JsonCodec

given Binder[Address] = Binder.jsonBinder
given Extractor[Address] = Extractor.jsonExtractor

case class TableTest(
    @PrimaryKey id: Long,
    @Index("name", 'A') @Index("nameRev", 'D') name: String,
    age: Int,
    address: Option[Address]
)
object TabulateTest extends TestSuite:
  val tests = Tests:
    test("tabulate"):
      val tbl = tabulate[TableTest]("table_test")
      val cols = tbl.cols
      assert(cols.length == 4)
      cols.foreach(col => println(col.name + " " + col.ordinal + " " + col.flags.mkString("|")))
  
