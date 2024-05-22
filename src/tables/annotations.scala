package tables

import scala.annotation.ConstantAnnotation

class PrimaryKey(pos: Int = 0) extends ConstantAnnotation

class Index(name: String, ord: 'A' | 'D', pos: Int = 0) extends ConstantAnnotation

enum Flag:
    case PrimaryKey(pos: Int)
    case Index(name: String, ord: 'A' | 'D', pos: Int)