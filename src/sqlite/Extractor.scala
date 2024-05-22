package sqlite

import zio.json.JsonDecoder
import zio.json.DecoderOps


@specialized
trait Extractor[+T]:
    def extract(pos: Int, sqlite: Sqlite): T

object Extractor:
    given intExtractor: Extractor[Int] with
        def extract(pos: Int, sqlite: Sqlite): Int = ???

    given longExtractor: Extractor[Long] with
        def extract(pos: Int, sqlite: Sqlite): Long = ???

    given stringExtractor: Extractor[String] with
            def extract(pos: Int, sqlite: Sqlite): String = ???

    given instantExtractor: Extractor[java.time.Instant] with
        def extract(pos: Int, sqlite: Sqlite): java.time.Instant = ???

    given optionExtractor[T: Extractor]: Extractor[Option[T]] with
        def extract(pos: Int, sqlite: Sqlite): Option[T] = 
            val v = summon[Extractor[T]]
            //TODO
            Some(v.extract(pos, sqlite))

    def jsonExtractor[T: JsonDecoder]: Extractor[T] = 
        new Extractor[T]:
            def extract(pos: Int, sqlite: Sqlite): T = 
                val stringExtractor = summon[Extractor[String]]
                val json = stringExtractor.extract(pos, sqlite)
                json.fromJson[T].fold(e => throw new Exception(e), identity)