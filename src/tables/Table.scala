package tables

import sqlite.Binder
import sqlite.Extractor

trait Query[T <: Tuple]:
    def join[T1 <: Tuple](other: Query[T1]): SqlParse[Tuple.Concat[T, T1], Query[Tuple.Concat[T, T1]]] = ???
    def filter: SqlParse[T, Query[T]] = ???
    private[tables] val tables: List[Table[?]]

trait Table[T] extends Query[Tuple1[T]]:
    val name: String
    private[tables] val cols: List[ColDef[?]]

object Table:
    def apply[T](name0: String, cols0: List[ColDef[?]]): Table[T] = 
        new Table[T]:
            val name = name0
            val cols = cols0
            val tables = List(this)
case class Tbl1(@PrimaryKey id: Long, name: String)
case class Tbl2(id: Long, @Index("name", 'A') @Index("nameRev", 'D') name: String, age: Int)

trait ColDef[T]:
    val name: String
    val ordinal: Int
    val binder: Binder[T]
    val extractor: Extractor[T]
    val flags: List[Flag]
    
val t1: Table[Tbl1] = tabulate[Tbl1]("tbl1")
val t2: Table[Tbl2] = tabulate[Tbl2]("tbl2")


// val q1 = t1.join(t2):
//     case (a, b) => a.toString() < b

// val q2 = t1.join(t2):
//     (a, b) => a.toString() < b

// val q3 = t1.join(t2):
//     t => t._1.toString() < t._2

val q4 = t1.join(t2):
     _.id == _.id