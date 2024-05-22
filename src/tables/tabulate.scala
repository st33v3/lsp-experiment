package tables

import sqlite.Binder
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import scala.compiletime.error
import sqlite.Extractor
import scala.reflect.ClassTag
import scala.quoted.*

inline def tabulate[T <: Product](name: String | Null)(using mirror: Mirror.ProductOf[T], tag: ClassTag[T]): Table[T] = 
    val cols = createFields[T, mirror.MirroredElemLabels, mirror.MirroredElemTypes](0)
    val nm = if name == null then tag.runtimeClass.getSimpleName.nn else name
    Table[T](nm, cols)

private[tables] inline def createFields[T <: Product, N <: Tuple, M <: Tuple](inline ord: Int): List[ColDef[?]] =
    inline erasedValue[(N, M)] match
        case _: (EmptyTuple, EmptyTuple) => 
            Nil
        case _: (n *: ns, m *: ms) =>            
            createField[T, n, m](ord) :: createFields[T, ns, ms](ord + 1)

private[tables] inline def createField[T <: Product, N, F](inline ord: Int) = 
    val n = constValue[N]
    new ColDef[F] {
        val name = n.toString()
        val ordinal = ord
        val binder = summonInline[Binder[F]]
        val extractor = summonInline[Extractor[F]]
        val flags = parseAnnotations[T, N]
    }
    
private [tables] inline def parseAnnotations[T, N]: List[Flag] = ${parseAnnotationsImpl[T, N]}

private [tables] def parseAnnotationsImpl[T: Type, N: Type](using Quotes): Expr[List[Flag]] = 
    import quotes.reflect.*
   
    inline def copyParams[T <: Tuple](xparams: List[Term]): Tuple.Map[T, Expr] = 
        inline erasedValue[T] match
            case _: EmptyTuple => 
                EmptyTuple
            case _: (t *: ts) => 
                xparams.head.asExprOf[t] *: copyParams[ts](xparams.tail)

    inline def annotationParams[T <: NonEmptyTuple](annot: Term) = 
        val Apply(Select(_, _), params) = annot : @unchecked
        copyParams[T](params)

    val ConstantType(StringConstant(n)) = TypeRepr.of[N] : @unchecked
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    val fieldSym = sym.primaryConstructor.paramSymss(0).find(_.name == n).getOrElse(report.errorAndAbort(s"Field $n not found"))
    val flags = fieldSym.annotations.collect: 
        case a if a.tpe =:= TypeRepr.of[PrimaryKey] =>
            val Tuple1(ord) = annotationParams[Tuple1[Int]](a)
            '{Flag.PrimaryKey($ord)}
        case a if a.tpe =:= TypeRepr.of[Index] =>
            val (name, dir, ord) = annotationParams[(String, 'A' | 'D', Int)](a)
            '{Flag.Index($name, $dir, $ord)}
    Expr.ofList(flags)

// transparent inline def copyParams[T <: Tuple](using q: Quotes)(params: List[q.reflect.Term]): Tuple = 
//     inline erasedValue[T] match
//         case _: EmptyTuple => 
//             EmptyTuple
//         case _: (t *: ts) => 
//             val head = params.head
//             val tpe = summonInline[Type[t]]
//             head.asExprOf[t](using tpe) *: copyParams[ts](params.tail)

