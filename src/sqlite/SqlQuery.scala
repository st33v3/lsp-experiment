package sqlite

import scala.collection.mutable.ListBuffer

sealed trait SqlParam:
  def bind(stmt: SqliteStmt, pos: Int): Int

class VerbatimParam(val value: String) extends SqlParam:
  def bind(stmt: SqliteStmt, pos: Int): Int = 0

class BindSqlParam[T](val value: T, val binder: Binder[T]) extends SqlParam:
    def bind(stmt: SqliteStmt, pos: Int): Int = 
      binder.bind(stmt, pos, value)
      1

class SqlQuery(val command: String, val params: List[BindSqlParam[?]])

object SqlParam:
  def verbatim(value: String): SqlParam = new VerbatimParam(value)
  def verbatimStr(value: String): SqlParam = new VerbatimParam("'" + value + "'")
  given binderToParam[T: Binder]: Conversion[T, SqlParam] = t => new BindSqlParam[T](t, summon[Binder[T]])  

object SqlQuery:
  extension (sc: StringContext)
    def sql(args: SqlParam*): SqlQuery =
      val bld = StringBuilder()
      val bpars = ListBuffer[BindSqlParam[?]]()
      bld ++= sc.parts.head
      sc.parts.tail.zip(args).foreach: (part, arg) =>
        arg match
          case b: VerbatimParam =>  bld ++= b.value
          case b: BindSqlParam[?] => 
            bld ++= "?"
            bpars += b
        bld ++= part      
      SqlQuery(bld.toString(), bpars.toList)


 
