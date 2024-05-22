package sqlite

import zio.json.JsonDecoder
import zio.json.DecoderOps
import java.time.Instant

@specialized
trait Extractor[+T]:
    def extract(stmt: SqliteStmt, pos: Int): T
    def allowNull: Boolean = false
    /**
     * See also https://www.sqlite.org/datatype3.html
     */
    def columnType: String 

object Extractor:
    given intExtractor: Extractor[Int] with
        def extract(stmt: SqliteStmt, pos: Int): Int = stmt.readInt(pos)
        def columnType = "INTEGER"

    given longExtractor: Extractor[Long] with
        def extract(stmt: SqliteStmt, pos: Int): Long = stmt.readLong(pos)
        def columnType = "BIGINT"

    given doubleExtractor: Extractor[Double] with
        def extract(stmt: SqliteStmt, pos: Int): Double = stmt.readDouble(pos)
        def columnType = "REAL"


    given stringExtractor: Extractor[String] with
        def extract(stmt: SqliteStmt, pos: Int): String = stmt.readString(pos)
        def columnType = "TEXT"

    given instantExtractor: Extractor[Instant] with
        def extract(stmt: SqliteStmt, pos: Int): Instant = Instant.from(SqliteLib.datetimeFormatter.parse(stmt.readString(pos))).nn
        def columnType = "TEXT"

    given optionExtractor[T](using base: Extractor[T]): Extractor[Option[T]] with
        def extract(stmt: SqliteStmt, pos: Int): Option[T] = 
            if stmt.isNull(pos) then None
            else Some(base.extract(stmt, pos))
        override def allowNull = true
        def columnType = base.columnType
        

    def jsonExtractor[T: JsonDecoder](using base: Extractor[String]): Extractor[T] = 
        new Extractor[T]:
            def extract(stmt: SqliteStmt, pos: Int): T = 
                val json = base.extract(stmt, pos)
                json.fromJson[T].fold(e => throw new Exception(e), identity)
            def columnType = base.columnType
            override def allowNull = base.allowNull