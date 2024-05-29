package sqlite

import java.lang.foreign.Linker
import java.lang.foreign.Arena
import java.lang.foreign.SymbolLookup
import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.ValueLayout
import java.lang.foreign.MemoryLayout
import java.lang.foreign.MemorySegment
import java.lang.invoke.MethodHandle

import scala.compiletime.erasedValue

import java.time.ZoneId
import java.time.format.DateTimeFormatter

opaque type SqliteOpen = Int
object SqliteOpen:
    extension (code: SqliteOpen)
        def toInt: Int = code

    val READONLY: SqliteOpen =         0x00000001  /* Ok for sqlite3_open_v2() */
    val READWRITE: SqliteOpen =        0x00000002  /* Ok for sqlite3_open_v2() */
    val CREATE: SqliteOpen =           0x00000004  /* Ok for sqlite3_open_v2() */
    val DELETEONCLOSE: SqliteOpen =    0x00000008  /* VFS only */
    val EXCLUSIVE: SqliteOpen =        0x00000010  /* VFS only */
    val AUTOPROXY: SqliteOpen =        0x00000020  /* VFS only */
    val URI: SqliteOpen =              0x00000040  /* Ok for sqlite3_open_v2() */
    val MEMORY: SqliteOpen =           0x00000080  /* Ok for sqlite3_open_v2() */
    val MAIN_DB: SqliteOpen =          0x00000100  /* VFS only */
    val TEMP_DB: SqliteOpen =          0x00000200  /* VFS only */
    val TRANSIENT_DB: SqliteOpen =     0x00000400  /* VFS only */
    val MAIN_JOURNAL: SqliteOpen =     0x00000800  /* VFS only */
    val TEMP_JOURNAL: SqliteOpen =     0x00001000  /* VFS only */
    val SUBJOURNAL: SqliteOpen =       0x00002000  /* VFS only */
    val SUPER_JOURNAL: SqliteOpen =    0x00004000  /* VFS only */
    val NOMUTEX: SqliteOpen =          0x00008000  /* Ok for sqlite3_open_v2() */
    val FULLMUTEX: SqliteOpen =        0x00010000  /* Ok for sqlite3_open_v2() */
    val SHAREDCACHE: SqliteOpen =      0x00020000  /* Ok for sqlite3_open_v2() */
    val PRIVATECACHE: SqliteOpen =     0x00040000  /* Ok for sqlite3_open_v2() */
    val WAL: SqliteOpen =              0x00080000  /* VFS only */
    val NOFOLLOW: SqliteOpen =         0x01000000  /* Ok for sqlite3_open_v2() */
    val EXRESCODE: SqliteOpen =        0x02000000  /* Extended result codes */

object SqliteResult:
    val SQLITE_OK = 0
    val SQLITE_ROW = 100
    val SQLITE_DONE = 101
    val SQLITE_ERROR = 1
    val SQLITE_BUSY = 5

object SqliteColumnType:
    val SQLITE_INTEGER = 1
    val SQLITE_FLOAT = 2
    val SQLITE_TEXT = 3
    val SQLITE_BLOB = 4
    val SQLITE_NULL = 5    

object SqliteLib:
    private val arena = Arena.ofAuto().nn
    private val linker = Linker.nativeLinker().nn
    private val lookup = SymbolLookup.libraryLookup("/usr/lib/x86_64-linux-gnu/libsqlite3.so.0", arena).nn
    private def find(name: String) = 
        val ret = lookup.find(name).nn
        if !ret.isPresent() then throw Exception(s"Symbol $name not found")
        ret.get().nn

    private inline def layout[T <: Tuple]: List[MemoryLayout] = inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts) => inline erasedValue[t] match 
            case _: Int => ValueLayout.JAVA_INT.nn :: layout[ts]
            case _: Long => ValueLayout.JAVA_LONG.nn :: layout[ts]
            case _: Double => ValueLayout.JAVA_DOUBLE.nn :: layout[ts]
            case _: MemorySegment => ValueLayout.ADDRESS.nn :: layout[ts]

    private inline def make[T <: Tuple](name: String): MethodHandle = 
        val l = layout[T]
        linker.downcallHandle(find(name), FunctionDescriptor.of(l.head, l.tail*).nn).nn

    val open_v2 = make[(Int, MemorySegment, MemorySegment, Int, MemorySegment)]("sqlite3_open_v2")
    
    /* int sqlite3_close_v2(sqlite3*); */
    val close_v2 = make[(Int, MemorySegment)]("sqlite3_close_v2")
    
    /* int sqlite3_prepare_v2(
      sqlite3 *db,            /* Database handle */
      const char *zSql,       /* SQL statement, UTF-8 encoded */
      int nByte,              /* Maximum length of zSql in bytes. */
      sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
      const char **pzTail     /* OUT: Pointer to unused portion of zSql */
    ); */
    val prepare_v2 = make[(Int, MemorySegment, MemorySegment, Int, MemorySegment, MemorySegment)]("sqlite3_prepare_v2")

    val finalizeStmt = make[(Int, MemorySegment)]("sqlite3_finalize")

    /* int sqlite3_extended_result_codes(sqlite3*, int onoff); */
    val extended_result_codes = make[(Int, MemorySegment, Int)]("sqlite3_extended_result_codes")

    /* int sqlite3_step(sqlite3_stmt*); */
    val step = make[(Int, MemorySegment)]("sqlite3_step")

    /* int sqlite3_bind_null(sqlite3_stmt*, int); */
    val bind_null = make[(Int, MemorySegment, Int)]("sqlite3_bind_null")

    /* int sqlite3_bind_double(sqlite3_stmt*, int, double); */
    val bind_double = make[(Int, MemorySegment, Int, Double)]("sqlite3_bind_double")

    /* int sqlite3_bind_int(sqlite3_stmt*, int, int); */
    val bind_int = make[(Int, MemorySegment, Int, Int)]("sqlite3_bind_int")

    /* int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64); */
    val bind_int64 = make[(Int, MemorySegment, Int, Long)]("sqlite3_bind_int64")

    /* int sqlite3_bind_text(sqlite3_stmt*,int,const char*,int,void(*)(void*)); */
    val bind_text = make[(Int, MemorySegment, Int, MemorySegment, Int, MemorySegment)]("sqlite3_bind_text")

    /* const char *sqlite3_errstr(int); */
    val errstr = make[(MemorySegment, Int)]("sqlite3_errstr")

    /* const char *sqlite3_errmsg(sqlite3*); */
    val errmsg = make[(MemorySegment, MemorySegment)]("sqlite3_errmsg")

    /* int sqlite3_error_offset(sqlite3 *db); */
    //val error_offset = make[(Int, MemorySegment)]("sqlite3_error_offset")

    /* int sqlite3_column_count(sqlite3_stmt *pStmt); */
    val column_count = make[(Int, MemorySegment)]("sqlite3_column_count")

    /* int sqlite3_column_bytes(sqlite3_stmt*, int iCol) */
    val column_bytes = make[(Int, MemorySegment, Int)]("sqlite3_column_bytes")

    /* double sqlite3_column_double(sqlite3_stmt*, int iCol); */
    val column_double = make[(Double, MemorySegment, Int)]("sqlite3_column_double")

    /* int sqlite3_column_int(sqlite3_stmt*, int iCol); */
    val column_int = make[(Int, MemorySegment, Int)]("sqlite3_column_int")

    /* sqlite3_int64 sqlite3_column_int64(sqlite3_stmt*, int iCol); */
    val column_int64 = make[(Long, MemorySegment, Int)]("sqlite3_column_int64")

    /* const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol); */
    val column_text = make[(MemorySegment, MemorySegment, Int)]("sqlite3_column_text")

    /* int sqlite3_column_type(sqlite3_stmt*, int iCol); */
    val column_type = make[(Int, MemorySegment, Int)]("sqlite3_column_type")


    def extractString(segment: MemorySegment, maxLength: Int, defStr: => String): String = 
        if (segment == MemorySegment.NULL) then defStr
        else 
            val seg = segment.reinterpret(maxLength).nn
            seg.getUtf8String(0).nn

    def checkResult(res: Int, db: MemorySegment): Unit = 
        res match
            case SqliteResult.SQLITE_OK => ()
            case _ if db != MemorySegment.NULL =>
                val msg = SqliteLib.errmsg.invokeExact(db): MemorySegment
                val msgStr = extractString(msg, 512, "Unknown error")
                throw new SqliteException(s"Error ($res): $msgStr")
            case _ =>
                val msg = SqliteLib.errstr.invokeExact(res): MemorySegment
                val msgStr = extractString(msg, 512, "Unknown error")
                throw new SqliteException(s"Error ($res): $msgStr")
    
    val datetimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS").nn.withZone(ZoneId.systemDefault()).nn

    val SQLITE_STATIC = MemorySegment.ofAddress(0L)
    val SQLITE_TRANSIENT = MemorySegment.ofAddress(-1L)


