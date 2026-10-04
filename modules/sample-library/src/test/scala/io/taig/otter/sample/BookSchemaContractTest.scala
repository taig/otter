package io.taig.otter.sample

import io.circe.Json
import io.circe.parser.parse
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.sample.api.schema
import zio.Scope
import zio.test.*

object BookSchemaContractTest extends ZIOSpecDefault:
  private def document(value: String): Json = parse(value).toOption.get

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BookSchemaContractTest")(
    test("title accepts lengths 1 and 200 and rejects 0 and 201"):
      val accepted =
        List("x", "x" * 200).forall(value => JsonCirceDecoder.decode(schema.title, Json.fromString(value)).isValid)
      val rejected =
        List("", "x" * 201).forall(value => JsonCirceDecoder.decode(schema.title, Json.fromString(value)).isInvalid)
      assertTrue(accepted, rejected)
    ,
    test("pages accepts 1 and rejects 0"):
      assertTrue(
        JsonCirceDecoder.decode(schema.pages, Json.fromInt(1)).isValid,
        JsonCirceDecoder.decode(schema.pages, Json.fromInt(0)).isInvalid
      )
    ,
    test("genres accepts ten entries and rejects eleven"):
      val ten = Json.fromValues(List.fill(10)(Json.fromString("poetry")))
      val eleven = Json.fromValues(List.fill(11)(Json.fromString("poetry")))
      assertTrue(
        JsonCirceDecoder.decode(schema.genres, ten).isValid,
        JsonCirceDecoder.decode(schema.genres, eleven).isInvalid
      )
    ,
    test("create defaults omitted genres and patch preserves omission"):
      val create = document(
        """{"isbn":"9780000000000","title":"A title","pages":1,"published":"2020-01-01"}"""
      )
      val patch = document("{}")

      assertTrue(
        JsonCirceDecoder.decode(schema.create, create).toOption.exists(_.genres.isEmpty),
        JsonCirceDecoder.decode(schema.patch, patch).toOption.contains(Book.Patch(None, None, None, None))
      )
  )
