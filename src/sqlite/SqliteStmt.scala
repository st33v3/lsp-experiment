package sqlite

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout
import scala.util.Using.resource

class SqliteStmt(private val stmt: MemorySegment, private val db: MemorySegment) extends AutoCloseable:
    import SqliteLib.*

    def step(): Boolean = 
        val res = SqliteLib.step.invokeExact(stmt): Int
        res & 255 match
            case SqliteResult.SQLITE_ROW => true
            case SqliteResult.SQLITE_DONE => false
            case _ => 
                checkResult(res, db)
                false

    def bindNull(pos: Int): Unit = 
        val res = bind_null.invokeExact(stmt, pos): Int
        checkResult(res, db)

    def bindInt(pos: Int, value: Int): Unit = 
        val res = bind_int.invokeExact(stmt, pos, value): Int
        checkResult(res, db)
    
    def bindLong(pos: Int, value: Long): Unit =
        val res = bind_int64.invokeExact(stmt, pos, value): Int
        checkResult(res, db)

    def bindDouble(pos: Int, value: Double): Unit = 
        val res = bind_double.invokeExact(stmt, pos, value): Int
        checkResult(res, db)

    def bindString(pos: Int, value: String): Unit = 
        resource(Arena.ofConfined().nn): arena =>
            val str = arena.allocateUtf8String(value).nn
            val res = bind_text.invokeExact(stmt, pos, str, str.byteSize().toInt, SQLITE_TRANSIENT): Int
            checkResult(res, db)

    def columnCount(): Int = column_count.invokeExact(stmt): Int

    def isNull(pos: Int): Boolean = 
        val res = column_type.invokeExact(stmt, pos): Int
        res == SqliteColumnType.SQLITE_NULL

    def readInt(pos: Int): Int = column_int.invokeExact(stmt, pos): Int
    def readLong(pos: Int): Long = column_int64.invokeExact(stmt, pos): Long
    def readDouble(pos: Int): Double = column_double.invokeExact(stmt, pos): Double
    def readString(pos: Int): String = 
        val size = column_bytes.invokeExact(stmt, pos): Int
        val ptr = column_text.invokeExact(stmt, pos): MemorySegment
        extractString(ptr, size + 1, "")
    
    def close(): Unit = 
        val res = finalizeStmt.invokeExact(stmt): Int
        checkResult(res, db)


