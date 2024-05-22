package sqlite

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout
import sqlite.SqliteLib.checkResult

class Sqlite(private val db: MemorySegment) extends AutoCloseable:
    
    def prepare(sql: String): SqliteStmt = 
            val arena = Arena.ofConfined().nn
            try
                val ptr = arena.allocate(ValueLayout.ADDRESS).nn
                val query = arena.allocateUtf8String(sql).nn
                val res = SqliteLib.prepare_v2.invokeExact(db, query, query.byteSize().toInt, ptr, MemorySegment.NULL): Int
                checkResult(res, db)
                val stmtPointer = ptr.get(ValueLayout.ADDRESS, 0).nn
                if stmtPointer.equals(MemorySegment.NULL) then throw new SqliteException("Failed to prepare statement")
                new SqliteStmt(stmtPointer, db)
            finally
                arena.close()

    def close(): Unit = 
        val res = SqliteLib.close_v2.invokeExact(db): Int
        checkResult(res, MemorySegment.NULL.nn)

class SqliteException(msg: String) extends Exception(msg)

object Sqlite:
    def open(path: String, opts: SqliteOpen*): Sqlite =
        val arena = Arena.ofConfined().nn
        try
            val ptr = arena.allocate(ValueLayout.ADDRESS).nn
            val file = arena.allocateUtf8String(path).nn
            val o = opts.foldLeft(0)(_ | _.toInt)
            var res = SqliteLib.open_v2.invokeExact(file, ptr, o, MemorySegment.NULL): Int
            val pointer = ptr.get(ValueLayout.ADDRESS, 0).nn
            checkResult(res, pointer)
            res = SqliteLib.extended_result_codes.invokeExact(pointer, 1): Int
            new Sqlite(pointer)
        finally
            arena.close()

object SqliteResult:
    val SQLITE_OK = 0
    val SQLITE_ROW = 100
    val SQLITE_DONE = 101
    val SQLITE_ERROR = 1
    val SQLITE_BUSY = 5
