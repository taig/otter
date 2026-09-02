package io.taig.otter

import cats.syntax.all.*
import io.circe.CursorOp
import io.circe.DecodingFailure
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object JsonCirceTest extends ZIOSpecDefault:
  /** Fails in every shape [[Violations]] can hold, at once.
    *
    *   - `counts` reports a bad key beside a bad value, which is two violations on one node.
    *   - `monday` nests two levels deep and reports two of its members.
    *   - `translations` reports a bad key beside a value whose own member failed, which is the [[Violations.Root]] that
    *     carries both its own violations and nested nodes.
    */
  private val schema =
    field("counts", dictionary.list(locale, int)) :*
      field("monday", collection.list(json.book)) :*
      field("translations", dictionary.list(locale, json.book))

  private val document = CirceJson.obj(
    "counts" := CirceJson.obj("de_DE" := "many"),
    "monday" := CirceJson.arr(CirceJson.obj("title" := 1, "pages" := "many", "read" := true)),
    "translations" := CirceJson.obj("de_DE" := CirceJson.obj("title" := "Dune", "pages" := 412, "read" := "yes"))
  )

  /** What [[document]] fails with. A `Valid` reports as nothing at all, which every assertion below fails on. */
  private val violations: Option[Violations] = JsonCirceDecoder.decode(schema, document).swap.toOption

  private val failures: List[DecodingFailure] = violations.toList.flatMap(JsonCirce.failures(_).toList)

  private val failure: Option[DecodingFailure] = violations.map(JsonCirce.failure)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceTest")(
    test("history: a path in reading order comes back as the history circe wants"):
      val ops = JsonCirce.history(List(Step.Field("monday"), Step.Index(0)))

      assertTrue(
        ops == List(CursorOp.DownN(0), CursorOp.DownField("monday")),
        DecodingFailure("nope", ops).pathToRootString == ".monday[0]".some
      )
    ,
    test("history: handing circe the path the right way round would turn it inside out"):
      val ops = JsonCirce.history(List(Step.Field("monday"), Step.Index(0)))
      assertTrue(DecodingFailure("nope", ops.reverse).pathToRootString == "[0].monday".some)
    ,
    test("failures: every violation in the tree, depth first"):
      assertTrue(
        failures.length == 6,
        failures.map(_.message) == List(
          "invalid locale: de_DE",
          "*.type int",
          "*.type int",
          "*.type string",
          "invalid locale: de_DE",
          "*.type boolean"
        ),
        failures.map(_.pathToRootString) == List(
          ".counts.de_DE".some,
          ".counts.de_DE".some,
          ".monday[0].pages".some,
          ".monday[0].title".some,
          ".translations.de_DE".some,
          ".translations.de_DE.read".some
        )
      )
    ,
    test("failures: a node's own violations come before the nodes below it"):
      val nested = failures.map(_.pathToRootString).dropWhile(_ =!= ".translations.de_DE".some)
      assertTrue(nested == List(".translations.de_DE".some, ".translations.de_DE.read".some))
    ,
    test("failure: the first node wins, and everything it holds is joined"):
      assertTrue(
        failure.map(_.pathToRootString) == ".counts.de_DE".some.some,
        failure.map(_.message) == "invalid locale: de_DE, *.type int".some,
        failure.map(_.history) == List(CursorOp.DownField("de_DE"), CursorOp.DownField("counts")).some
      )
    ,
    test("failure: a node holding one violation is reported on its own"):
      val document = CirceJson.obj("title" := "Dune", "pages" := "many", "read" := true)
      val failure = JsonCirceDecoder.decode(json.book, document).swap.toOption.map(JsonCirce.failure)

      assertTrue(failure.map(_.message) == "*.type int".some, failure.map(_.pathToRootString) == ".pages".some.some)
    ,
    test("a Primitive.Text.Format reports its hint, while its constraint stays the type it is"):
      val result = JsonCirceDecoder.decode(locale, CirceJson.fromString("de_DE")).swap.toOption

      val constraint = result.collect { case Violations.Root(_, violations) => violations.head.constraint }

      assertTrue(
        result.map(JsonCirce.failure).map(_.message) == "invalid locale: de_DE".some,
        constraint == (Constraint.Generic.Type("locale"): Constraint).some,
        constraint.map(_.show) == "*.type locale".some
      )
    ,
    test("decoder: a valid document round trips"):
      val document = CirceJson.obj("title" := "Dune", "pages" := 412, "read" := true)
      assertTrue(JsonCirce.decoder(json.book).decodeJson(document) == Book("Dune", 412, true).asRight)
    ,
    test("decoder: an invalid document fails through apply with the single failure"):
      assertTrue(JsonCirce.decoder(schema).decodeJson(document) == failure.toLeft(()))
    ,
    test("decoder: decodeAccumulating reports every failure"):
      val result = JsonCirce.decoder(schema).decodeAccumulating(document.hcursor)
      assertTrue(result.leftMap(_.toList) == failures.invalid, result.swap.toOption.map(_.length) == 6.some)
    ,
    test("decoder: a caller's own translation replaces the single failure, not the accumulated ones"):
      val decoder = JsonCirce.decoder(schema, _ => DecodingFailure("nope", Nil))

      assertTrue(
        decoder.decodeJson(document) == DecodingFailure("nope", Nil).asLeft,
        decoder.decodeAccumulating(document.hcursor).leftMap(_.toList) == failures.invalid
      )
    ,
    test("encoder: agrees with the interpreter it is built on"):
      val book = Book("Dune", 412, true)
      assertTrue(JsonCirce.encoder(json.book)(book) == JsonCirceEncoder.encode(json.book, book))
  )
