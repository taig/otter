package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.JsonSyntax.*
import zio.*
import zio.test.*
import io.taig.otter.Json

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
        "properties" := List(
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

      assertTrue(input == result)
  )
