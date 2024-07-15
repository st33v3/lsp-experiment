
package incpeg

trait Recognizer:
    def validate(): Option[String]
    def recognize(input: Input): String | Null

object Recognizer:
    case object Ident extends Recognizer:
        def validate(): Option[String] = None
        def recognize(input: Input): String | Null = null
    case object Number extends Recognizer:
        def validate(): Option[String] = None
        def recognize(input: Input): String | Null = null