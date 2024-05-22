
package incpeg

trait Recognizer:
    def validate(): Option[String]
    def recognize(input: Input): String | Null
