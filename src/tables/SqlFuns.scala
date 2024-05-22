package tables


def compileTimeOnly() = throw new Exception("SQL functions are only available at compile time")

object SqlFuns:
    def between[T](value: T, start: T, end: T): Boolean = compileTimeOnly()
    def like(value: String, pattern: String): Boolean = compileTimeOnly()
    def isNull(value: Any): Boolean = compileTimeOnly()