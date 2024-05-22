package sqlite

import java.lang.foreign.Linker
import java.lang.foreign.Arena
import java.lang.foreign.SymbolLookup
import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.ValueLayout
import java.lang.foreign.MemoryLayout
import scala.compiletime.erasedValue
import java.lang.foreign.MemorySegment
import java.lang.invoke.MethodHandle

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
            case _: Long => ValueLayout.JAVA_DOUBLE.nn :: layout[ts]
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

    /* const char *sqlite3_errstr(int); */
    val errstr = make[(MemorySegment, Int)]("sqlite3_errstr")

    /* const char *sqlite3_errmsg(sqlite3*); */
    val errmsg = make[(MemorySegment, MemorySegment)]("sqlite3_errmsg")

    /* int sqlite3_error_offset(sqlite3 *db); */
    //val error_offset = make[(Int, MemorySegment)]("sqlite3_error_offset")

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

