package bench

import incpeg.RowCol
import incpeg.Source

trait Derivation:
    def pos: Result[RowCol]
    def additive: Result[BigInt]
    def multitive: Result[BigInt]
    def primary: Result[BigInt]
    def decimal: Result[BigInt]
    def char: Result[Char]

enum Result[+T]:
    case Parsed(value: T, next: Derivation) extends Result[T]
    case NoParse extends Result[Nothing]

/**
  * Rewrite of PEG parser from original paper to Scala.
  */
class OriginalPegParser(src: Source):
    import Result.*

    protected def parseMultitive(d: Derivation): Result[BigInt] =
        val alt1 = d.primary match
            case Parsed(left, d2) => d2.char match
                case Parsed('*', d3) => d3.multitive match
                    case Parsed(right, d4) => Parsed(left * right, d4)
                    case _ => NoParse
                case _ => NoParse
            case _ => NoParse
        if alt1 != NoParse then alt1 else d.primary


    protected def parsePrimary(d: Derivation): Result[BigInt] = 
        val alt1 = d.char match
            case Parsed('(', d2) => d2.additive match
                case Parsed(res, d3) => d3.char match
                    case Parsed(')', d4) => Parsed(res, d4)
                    case _ => NoParse
                case _ => NoParse
            case _ => NoParse
        if alt1 != NoParse then alt1 else d.decimal

    protected def parseDecimal(d: Derivation): Result[BigInt] =
        d.char match
            case Parsed(ch, d2) if ch >= '0' && ch <= '9' => d2.decimal match
                case Parsed(res, d3) => Parsed(res * 10 + (ch - '0'), d3)
                case _ => Parsed((ch - '0'), d2)
            case _ => NoParse

    protected def parseAdditive(d: Derivation): Result[BigInt] = 
        val alt1 = d.multitive match
            case Parsed(left, d2) => d2.char match
                case Parsed('+', d3) => d3.additive match
                    case Parsed(right, d4) => Parsed(left + right, d4)
                    case _ => NoParse
                case Parsed('-', d3) => d3.additive match
                    case Parsed(right, d4) => Parsed(left - right, d4)
                    case _ => NoParse
                case _ => NoParse
            case _ => NoParse
        if alt1 != NoParse then alt1 else d.multitive            
        

    def parse(pos0: RowCol): Derivation = 
        new Derivation:
            val pos = Parsed(pos0, this)
            lazy val additive = parseAdditive(this)
            lazy val multitive = parseMultitive(this)
            lazy val primary = parsePrimary(this)
            lazy val decimal = parseDecimal(this)
            lazy val char = if pos0 != RowCol.invalid then Parsed(src.charAt(pos0), parse(src.nextPos(pos0))) else NoParse

    def parseExpr(): BigInt = parse(RowCol.zero).additive match
        case Parsed(res, _) => res
        case _ => throw new Exception("Expected expression")  
