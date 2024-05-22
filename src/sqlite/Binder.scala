package sqlite

import zio.json._



@specialized
trait Binder[-T]:
    def bind(pos: Int, sqlite: Sqlite, value: T): Unit

object Binder:
    given intBinder: Binder[Int] with
        def bind(pos: Int, sqlite: Sqlite, value: Int): Unit = ???

    given longBinder: Binder[Long] with
        def bind(pos: Int, sqlite: Sqlite, value: Long): Unit = ???

    given stringBinder: Binder[String] with
        def bind(pos: Int, sqlite: Sqlite, value: String): Unit = ???

    given instantBinder: Binder[java.time.Instant] with
        def bind(pos: Int, sqlite: Sqlite, value: java.time.Instant): Unit = ???


    given optionBinder[T: Binder]: Binder[Option[T]] with
        def bind(pos: Int, sqlite: Sqlite, value: Option[T]): Unit = 
            val v = summon[Binder[T]]
            value match
                case Some(vl) => v.bind(pos, sqlite, vl)
                case None => ???

    def jsonBinder[T: JsonEncoder]: Binder[T] = 
        new Binder[T]:
            def bind(pos: Int, sqlite: Sqlite, value: T): Unit = 
                val json = value.toJson
                val stringBinder = summon[Binder[String]]
                stringBinder.bind(pos, sqlite, json)