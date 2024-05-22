package sqlite

import zio.json._
import sqlite.SqliteStmt
import sqlite.SqliteLib

@specialized
trait Binder[-T]:
    def bind(stmt: SqliteStmt, pos: Int, value: T): Unit

object Binder:
    given intBinder: Binder[Int] with
        def bind(stmt: SqliteStmt, pos: Int, value: Int): Unit =
            stmt.bindInt(pos, value)

    given longBinder: Binder[Long] with
        def bind(stmt: SqliteStmt, pos: Int, value: Long): Unit =
            stmt.bindLong(pos, value)

    given stringBinder: Binder[String] with
        def bind(stmt: SqliteStmt, pos: Int, value: String): Unit =
            stmt.bindString(pos, value)

    given instantBinder: Binder[java.time.Instant] with
        def bind(stmt: SqliteStmt, pos: Int, value: java.time.Instant): Unit =
            val fmt = SqliteLib.datetimeFormatter.format(value).nn
            stmt.bindString(pos, fmt)

    given optionBinder[T: Binder]: Binder[Option[T]] with
        def bind(stmt: SqliteStmt, pos: Int, value: Option[T]): Unit = 
            val v = summon[Binder[T]]
            value match
                case Some(vl) => v.bind(stmt, pos, vl)
                case None => stmt.bindNull(pos)

    def jsonBinder[T: JsonEncoder]: Binder[T] = 
        new Binder[T]:
            def bind(stmt: SqliteStmt, pos: Int, value: T): Unit = 
                val json = value.toJson
                val stringBinder = summon[Binder[String]]
                stringBinder.bind(stmt, pos, json)