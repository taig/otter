package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Borer
import io.bullet.borer.Dom
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Step
import io.taig.otter.component.JsonComponent.*
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
    Try(BorerDoc.toBorer(Doc.Num(lexeme))).toEither.left.map:
      case error: Borer.Error[?] => error.getMessage
      case error                 => error.toString

  /** One name declared twice, which is legal and read in arrival order by whoever holds the document in that order. */
  private val duplicated: Json.Record[(Int, Int)] = field("x", int) :* field("x", int)

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
          JsonBorerDecoder.decode(json.omittedTag, BorerDoc.toBorer(doc)) == Validated.valid(Note("Dune", 1.some)),
          JsonCirceDecoder.decode(json.omittedTag, CirceDoc.toCirce(doc)) == Validated.valid(Note("Messiah", 1.some))
        )
      ,
      test("a dictionary keeps it, where circe's object has already collapsed it"):
        val doc = Doc.Obj(List("1" -> Doc.Str("a"), "1" -> Doc.Str("a"), "2" -> Doc.Str("b")))

        assertTrue(
          JsonBorerDecoder.decode(json.printings, BorerDoc.toBorer(doc)) ==
            Validated.valid(List(1 -> "a", 1 -> "a", 2 -> "b")),
          JsonCirceDecoder.decode(json.printings, CirceDoc.toCirce(doc)) == Validated.valid(List(1 -> "a", 2 -> "b"))
        )
      ,
      /** The same divergence from the schema's side, which is what makes it a capability here rather than an accident.
        * `RecordEncoder` writes a member per declaration and `BorerWrite`'s `Monoid` is left to right, so borer writes
        * both and reads both back in the order they were declared.
        *
        * circe's `JsonObject` is a `LinkedHashMap` keyed by name and cannot hold two entries under one key at all, so
        * the write collapses to the last and the second field then reads as missing under a key the document plainly
        * has. That is the data type and not a setting: `JawnParser`'s `allowDuplicateKeys` only chooses between keeping
        * the last and refusing the document outright, and neither preserves one.
        */
      test("a schema naming one field twice round trips here, where circe collapses what it wrote"):
        val roundTrip = JsonCirceInterpreter.roundTrip(duplicated, (1, 2))

        assertTrue(
          JsonBorerInterpreter.encode(duplicated, (1, 2)) == """{"x":1,"x":2}""",
          JsonBorerInterpreter.roundTrip(duplicated, (1, 2)) == Validated.valid((1, 2)),
          JsonCirceInterpreter.encode(duplicated, (1, 2)) == """{"x":2}""",
          roundTrip.leftMap(violations.constraints) == Validated.invalid(List(Constraint.Generic.Required)),
          roundTrip.leftMap(violations.paths) == Validated.invalid(List(List(Step.Field("x"))))
        )
    )
  )
