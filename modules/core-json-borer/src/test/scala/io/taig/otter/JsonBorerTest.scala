package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Borer
import io.bullet.borer.Json as BorerJson
import io.taig.otter.codec.JsonBorerDecoder
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

import java.nio.charset.StandardCharsets.UTF_8

/** The bridge, which is the part of this module with no circe counterpart to agree with.
  *
  * `JsonBorer.failures`, `message`, `render` and `validated` are new code rather than a transliteration, because
  * borer's `Borer.Error` has room for a message and an input position and nothing else. So they are asserted
  * absolutely, against the same deliberately multi shaped document `JsonCirceTest` uses.
  */
object JsonBorerTest extends ZIOSpecDefault:
  /** Fails in every shape [[Violations]] can hold, at once, exactly as `JsonCirceTest`'s does.
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

  private val document = Doc.Obj(
    List(
      "counts" -> Doc.Obj(List("de_DE" -> Doc.Str("many"))),
      "monday" -> Doc.Arr(
        List(Doc.Obj(List("title" -> Doc.Num("1"), "pages" -> Doc.Str("many"), "read" -> Doc.Bool(true))))
      ),
      "translations" -> Doc.Obj(
        List(
          "de_DE" -> Doc.Obj(
            List("title" -> Doc.Str("Dune"), "pages" -> Doc.Num("412"), "read" -> Doc.Str("yes"))
          )
        )
      )
    )
  )

  /** What [[document]] fails with. A `Valid` reports as nothing at all, which every assertion below fails on. */
  private val violations: Option[Violations] =
    JsonBorerDecoder.decode(schema, BorerDoc.toBorer(document)).swap.toOption

  private val failures: List[String] = violations.toList.flatMap(JsonBorer.failures(_).toList)

  private val message: Option[String] = violations.map(JsonBorer.message)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerTest")(
    test("render: a path in reading order reads the way it reads in a document"):
      assertTrue(
        JsonBorer.render(List(Step.Field("monday"), Step.Index(0))) == ".monday[0]",
        JsonBorer.render(Nil) == ""
      )
    ,
    test("failures: every violation in the tree, depth first, each carrying its path"):
      assertTrue(
        failures.length == 6,
        failures == List(
          ".counts.de_DE: invalid locale: de_DE",
          ".counts.de_DE: *.type int",
          ".monday[0].pages: *.type int",
          ".monday[0].title: *.type string",
          ".translations.de_DE: invalid locale: de_DE",
          ".translations.de_DE.read: *.type boolean"
        )
      )
    ,
    test("message: the first node, carrying every failure found at it"):
      assertTrue(message == ".counts.de_DE: invalid locale: de_DE, *.type int".some)
    ,
    test("validated: reports every violation, and never fails"):
      val decoded = BorerJson
        .decode(Doc.render(document).getBytes(UTF_8))
        .to(using JsonBorer.validated(schema))
        .valueEither

      assertTrue(decoded.map(_.swap.toOption.map(JsonBorer.failures(_).length)) == Right(6.some))
    ,
    test("decoder: reports the first failure, as the error borer's own callers catch"):
      val decoded = BorerJson
        .decode(Doc.render(document).getBytes(UTF_8))
        .to(using JsonBorer.decoder(schema))
        .valueEither

      assertTrue(
        decoded.left.toOption
          .collect { case error: Borer.Error.ValidationFailure[?] => error.getMessage }
          .exists:
            _.contains(".counts.de_DE: invalid locale: de_DE, *.type int")
      )
    ,
    test("decoder: a caller's own translation replaces the message and nothing else"):
      val decoded = BorerJson
        .decode(Doc.render(document).getBytes(UTF_8))
        .to(using JsonBorer.decoder(schema, violations => s"${JsonBorer.failures(violations).length} problems"))
        .valueEither

      assertTrue(decoded.left.toOption.map(_.getMessage).exists(_.contains("6 problems")))
    ,
    test("decoder: a document that reads reports nothing"):
      val text = """{"title":"Dune","pages":412,"read":true}"""

      assertTrue(
        BorerJson.decode(text.getBytes(UTF_8)).to(using JsonBorer.decoder(json.book)).valueEither ==
          Right(Book("Dune", 412, true))
      )
    ,
    test("encoder: writes the document, in declaration order"):
      assertTrue(
        BorerJson.encode(Book("Dune", 412, true))(using JsonBorer.encoder(json.book)).toUtf8String ==
          """{"title":"Dune","pages":412,"read":true}"""
      )
    ,
    test("a document with no violations decodes to the value"):
      val book = Doc.Obj(List("title" -> Doc.Str("Dune"), "pages" -> Doc.Num("412"), "read" -> Doc.Bool(true)))

      assertTrue(JsonBorerDecoder.decode(json.book, BorerDoc.toBorer(book)) == Validated.valid(Book("Dune", 412, true)))
  )
