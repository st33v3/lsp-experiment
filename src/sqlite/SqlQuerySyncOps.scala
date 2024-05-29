package sqlite

import scala.util.Using.resource
import scala.collection.mutable.ListBuffer

object SqlQuerySyncOps:
  extension (query: SqlQuery)
    def bind(stmt: SqliteStmt): Unit =
        query.params.zipWithIndex.foreach { case (param, idx) => param.bind(stmt, idx + 1) }
    
    private def execStmt(stmt0: SqliteStmt): Unit =
        resource(stmt0): stmt =>
          bind(stmt)
          if (stmt.step()) throw new SqliteException("Expected no results")

    def exec()(using xaction: Transaction): Unit = execStmt(xaction.prepare(query.command))

    def execDb(db: Sqlite): Unit = execStmt(db.prepare(query.command))

    private def queryStmt[T](stmt0: SqliteStmt, reader: Reader[T]): List[T] =
        resource(stmt0): stmt =>
          bind(stmt)
          val bld = ListBuffer.empty[T]
          while stmt.step() do bld += reader.read(stmt)
          bld.toList

    def query[T : Reader]()(using xaction: Transaction): List[T] = queryStmt(xaction.prepare(query.command), summon[Reader[T]])

    def queryDb[T : Reader](db: Sqlite): List[T] = queryStmt(db.prepare(query.command), summon[Reader[T]])


