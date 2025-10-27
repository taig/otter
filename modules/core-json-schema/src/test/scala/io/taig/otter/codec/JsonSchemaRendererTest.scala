package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.AnnotatedSyntax.*
import io.taig.otter.syntax.JsonSyntax.*
import zio.*
import zio.test.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.JsonSchemaKeys

object JsonSchemaRendererTest extends ZIOSpecDefault:
  enum Animal:
    case Bird
    case Cat
    case Dog

  object Animal:
    val json: Json.Enumeration[Animal] = enumeration(string):
      case Bird => "bird"
      case Cat  => "cat"
      case Dog  => "dog"

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaEncoderTest")(
    test("sample"):
      val input = JsonSchemaRenderer(encoder = CirceJsonEncoder).render(
        field("name", string) :*
          field("age", int).optional :*
          field("height", float).optional(default = 1.60f) :*
          field("probability", bigDecimal.coerce) :*
          field("country", constant(string, "de")) :*
          field("scores", collection.list(int)) :*
          field("animal", Animal.json)
      )

      val result = CirceJson.obj(
        "type" := "object",
        "properties" := CirceJson.obj(
          "name" := CirceJson.obj(
            "type" := "string"
          ),
          "age" := CirceJson.obj(
            "type" := "integer"
          ),
          "height" := CirceJson.obj(
            "type" := "number",
            "default" := 1.60f
          ),
          "probability" := CirceJson.obj(
            "type" := "string"
          ),
          "country" := CirceJson.obj(
            "const" := "de"
          ),
          "scores" := CirceJson.obj(
            "type" := "array",
            "items" := CirceJson.obj(
              "type" := "integer"
            )
          ),
          "animal" := CirceJson.obj(
            "type" := "string",
            "enum" := List("bird", "cat", "dog")
          )
        ),
        "required" := List("name", "probability", "country", "scores", "animal")
      )

      assertTrue(input == result),
    test("refs"):
      val input = JsonSchemaRenderer(encoder = CirceJsonEncoder).render(
        field("name", string) :*
          field("age", int.attr(Keys.name, "age")).optional
      )

      val result = CirceJson.obj(
        "type" := "object",
        "properties" := CirceJson.obj(
          "name" := CirceJson.obj(
            "type" := "string"
          ),
          "age" := CirceJson.obj(
            "$ref" := "#/$defs/age"
          )
        ),
        "required" := List("name"),
        "$defs" := CirceJson.obj(
          "age" := CirceJson.obj(
            "type" := "integer"
          )
        )
      )

      assertTrue(input == result),
    test("refs: field default"):
      val input = JsonSchemaRenderer(encoder = CirceJsonEncoder).render(
          field("age", int.attr(JsonSchemaKeys.name, "age")).optional(default = 18).toRecord
      )

      val result = CirceJson.obj(
        "type" := "object",
        "properties" := CirceJson.obj(
          "age" := CirceJson.obj(
            "$ref" := "#/$defs/age",
            "default" := 18
          )
        ),
        "required" := CirceJson.arr(),
        "$defs" := CirceJson.obj(
          "age" := CirceJson.obj(
            "type" := "integer"
          )
        )
      )

      assertTrue(input == result)
  )
