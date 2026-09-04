package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Borer
import io.bullet.borer.Dom
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

import scala.util.Try

/** The places this module does *not* answer as the circe one does, asserted as divergences.
  *
  * [[JsonBorerAgreementTest]] leaves these out of its corpus, where they could only report that a known difference is
  * still there. Here they are stated the other way round: each one fails if it ever stops being true, which is the
  * alarm that goes off when borer changes its parser under us or when a fix makes one of them go away.
  */
object JsonBorerDivergenceTest extends ZIOSpecDefault:
  private def borer(lexeme: String): Either[String, Dom.Element] =
    Try(Doc.toBorer(Doc.Num(lexeme))).toEither.left.map:
      case error: Borer.Error[?] => error.getMessage
      case error                 => error.toString

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerDivergenceTest")(
    suite("borer's parser refuses an exponent circe reads")(
      /** borer caps an absolute exponent at `Json.DecodingConfig.maxNumberAbsExponent`, 64 by default, and refuses the
        * document outright. circe reads it and only refuses later, at `toBigInteger`, under its own
        * `bigIntegerMaxDigits` cap. Both libraries stop a twelve character document from asking for a 256MB integer;
        * they stop it in different places. A caller who needs the larger exponents raises borer's own limit, which is a
        * decoding config rather than anything this module holds.
        */
      DocTest.Divergent.map(lexeme => test(lexeme)(assertTrue(borer(lexeme).left.exists(_.contains("exponent")))))*
    ),
    suite("a duplicated key")(
      /** Both read one of the two, and not the same one. `Fields.take` hands out the first unclaimed occurrence,
        * because that is what reading a document in arrival order means; circe's `JsonObject` has already collapsed the
        * key by the time a schema sees it, keeping the last value written. Neither is wrong -- JSON does not say -- and
        * a document with a duplicated key is not one either module promises anything about.
        */
      test("a record reads the first occurrence where circe reads the last"):
        val doc = Doc.Obj(List("title" -> Doc.Str("Dune"), "title" -> Doc.Str("Messiah"), "tag" -> Doc.Num("1")))

        assertTrue(
          JsonBorerDecoder.decode(json.omittedTag, Doc.toBorer(doc)) == Validated.valid(Note("Dune", 1.some)),
          JsonCirceDecoder.decode(json.omittedTag, Doc.toCirce(doc)) == Validated.valid(Note("Messiah", 1.some))
        )
      ,
      test("a dictionary keeps it, where circe's object has already collapsed it"):
        val doc = Doc.Obj(List("1" -> Doc.Str("a"), "1" -> Doc.Str("a"), "2" -> Doc.Str("b")))

        assertTrue(
          JsonBorerDecoder.decode(json.printings, Doc.toBorer(doc)) ==
            Validated.valid(List(1 -> "a", 1 -> "a", 2 -> "b")),
          JsonCirceDecoder.decode(json.printings, Doc.toCirce(doc)) == Validated.valid(List(1 -> "a", 2 -> "b"))
        )
    )
  )
