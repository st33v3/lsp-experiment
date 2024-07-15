package incpeg.recognizers

import incpeg.Recognizer
import incpeg.Input
import incpeg.CSTNode

class ConstRecognizer(val value: String) extends Recognizer {
  def validate(): Option[String] = value.length() match
    case 0 => Some("ConstRecognizer value cannot be empty")
    case _ => None

  def recognize(input: Input): String | Null = {
    var pos = 0
    while pos < value.length() && input.current == value.charAt(pos) do
      input.consume()
      pos += 1
    if (pos == value.length()) then value
    else null
  }
  
}
