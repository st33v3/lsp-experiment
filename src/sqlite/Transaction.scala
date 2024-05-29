package sqlite

case class Transaction(private val db: Sqlite):
    def prepare(sql: String): SqliteStmt = db.prepare(sql)
    def exec(sql: String): Unit = db.exec(sql)
    def commit(): Unit = db.commitTransaction()
    def rollback(): Unit = db.rollbackTransaction()

def transactionally[T](db: Sqlite)(f: Transaction ?=> T): T =
    db.startTransaction()
    val xaction = Transaction(db)
    try
        val res = f(using xaction)
        xaction.commit()
        res
    catch
        case e: Exception =>
            xaction.rollback()
            throw e
