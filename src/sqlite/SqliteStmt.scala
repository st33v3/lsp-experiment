package sqlite

import java.lang.foreign.MemorySegment
import java.lang.foreign.Arena
import java.lang.foreign.ValueLayout
import sqlite.SqliteLib.checkResult

class SqliteStmt(private val stmt: MemorySegment, private val db: MemorySegment) extends AutoCloseable:
    
    def step(): Boolean = 
        val res = SqliteLib.step.invokeExact(stmt): Int
        res & 255 match
            case SqliteResult.SQLITE_ROW => true
            case SqliteResult.SQLITE_DONE => false
            case _ => 
                checkResult(res, db)
                false

    def bindNull(pos: Int): Unit = 
        val res = SqliteLib.bind_null.invokeExact(stmt, pos): Int
        checkResult(res, db)

    def close(): Unit = 
        val res = SqliteLib.finalizeStmt.invokeExact(stmt): Int
        checkResult(res, db)


