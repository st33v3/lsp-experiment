package tables

import utest._
import sqlite.Sqlite
import sqlite.SqliteOpen
import scala.util.Using


object SqliteTest extends TestSuite:
  val tests = Tests:
    test("sqlite open"):
      Using.resource(Sqlite.open("test.db", SqliteOpen.CREATE, SqliteOpen.READWRITE, SqliteOpen.SHAREDCACHE, SqliteOpen.FULLMUTEX)): sqlite =>
        val stmt = sqlite.prepare("CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT, age INTEGER, address TEXT)")
        stmt.step()
        stmt.close()
        ()
  
