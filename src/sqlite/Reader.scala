package sqlite

import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.summonInline

trait Reader[+T]: 
  def read(stmt: SqliteStmt, base: Int = 0): T

object Reader:

    given emptyTupleReader[T <: EmptyTuple]: Reader[EmptyTuple] with
        def read(stmt: SqliteStmt, base: Int): EmptyTuple = EmptyTuple

    given tupleReader[H, T <: Tuple](using head: Extractor[H], tail: Reader[T]): Reader[H *: T] with
        def read(stmt: SqliteStmt, base: Int): H *: T = head.extract(stmt, base) *: tail.read(stmt, base + 1)

    inline def derived[T <: Product](using m: Mirror.ProductOf[T]): Reader[T] = 
        val extractors = deriveElem[m.MirroredElemTypes]
        new Reader[T]:
            def read(stmt: SqliteStmt, base: Int): T = 
                val vals = readElems(stmt, extractors, base)
                m.fromProduct(vals)
    
    private inline def deriveElem[T <: Tuple]: Tuple.Map[T, Extractor] = 
        inline erasedValue[T] match
            case _: EmptyTuple => EmptyTuple
            case _: (t *: ts) => 
                summonInline[Extractor[t]] *: deriveElem[ts]

    private def readElems[T <: Tuple](stmt: SqliteStmt, extractors: T, idx: Int): Tuple = 
        extractors: @unchecked match
            case EmptyTuple => EmptyTuple
            case ((h : Extractor[?]) *: t) => 
                h.extract(stmt, idx) *: readElems(stmt, t, idx + 1)
        
// TODO should use m.fromProductTyped, Tuple.InverseMap and Tuple.isMappedBy to have type safe access to the fields
// I coould not make it work, not sure that it is not a bug in compiler, so I used the not type safe version

//def f[T](e: T): Option[T] = ???

//inline def hu[T <: Tuple : Tuple.IsMappedBy[Extractor]](tup: T): Tuple.InverseMap[T, Extractor] = tup.map([T] => t => Option(t))
    // inline tup match
    //     case _: EmptyTuple => EmptyTuple
    //     case x: (Extractor[t] *: ts) => 
    //         inline x match
    //             case (h *: t) => h.extract(null, 0) *: hu(t)
