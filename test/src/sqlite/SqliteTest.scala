package tables

import utest._
import sqlite.Sqlite
import sqlite.Reader
import sqlite.SqlQuery.*
import sqlite.SqlQuerySyncOps.*
import sqlite.SqliteOpen
import scala.util.Using
import sqlite.transactionally
import sqlite.SqlParam

case class SqliteObject(name: String, dbtype: String, tblName: String, sql: String) derives Reader

case class SqliteTableCol(cid: Int, name: String, dbtype: String, notNull: Boolean, defaultValue: String, primaryKey: Int) derives Reader

case class SqliteIndex(iid: Int, name: String, unique: Boolean, origin: String, partial: Boolean) derives Reader

case class SqliteIndexCol(icid: Int, cid: Int, name: Option[String], reverse: Boolean, collation: String, notAux: Boolean) derives Reader

case class TestTable(id: Long, name: String, age: Int, address: String) derives Reader


object SqliteTest extends TestSuite:
  var db: Sqlite = Sqlite.open("test.db", SqliteOpen.CREATE, SqliteOpen.READWRITE, SqliteOpen.SHAREDCACHE, SqliteOpen.FULLMUTEX)
  db.exec("CREATE TABLE IF NOT EXISTS test (id INTEGER not null PRIMARY KEY, name TEXT not null, age INTEGER not null, address TEXT null)")
  db.exec("CREATE INDEX IF NOT EXISTS test_name on test (name desc)")
  db.exec("CREATE UNIQUE INDEX IF NOT EXISTS test_address on test (address) where address is not null")
  
  override def utestAfterAll(): Unit = 
    db.exec("DROP TABLE IF EXISTS test")
    db.close()

  val tests = Tests:
  
    test("read projected schema"):
      val schema = sql"select name, type from sqlite_schema".queryDb[(String, String)](db)
      println(schema)

    test("read schema"):
      val schema = sql"select name, type, tbl_name, sql from sqlite_schema".queryDb[SqliteObject](db)
      println(schema)
      schema.filter(_.dbtype == "table").foreach: table =>
        val columns = sql"pragma table_info(${SqlParam.verbatimStr(table.name)})".queryDb[SqliteTableCol](db)
        println(columns)
        val indices = sql"pragma index_list(${SqlParam.verbatimStr(table.name)})".queryDb[SqliteIndex](db)
        println(indices)
        indices.foreach: index =>
          val indexCols = sql"pragma index_xinfo(${SqlParam.verbatimStr(index.name)})".queryDb[SqliteIndexCol](db)
          println(indexCols)

    test("insert in transaction"):
      val age1 = 30
      val age2 = 25
      transactionally(db):
        sql"insert into test (name, age, address) values ('John', ${age1}, '123 Main St')".exec()
        sql"insert into test (name, age, address) values ('Jane', ${age2}, '456 Elm St')".exec()

      val tests = sql"select * from test".queryDb[TestTable](db) // Fragile, depends on correct order of columns insertion order
      assert(tests.length == 2)
      assert(tests(0) == TestTable(1, "John", 30, "123 Main St"))
      assert(tests(1) == TestTable(2, "Jane", 25, "456 Elm St"))



  
