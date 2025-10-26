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
        field("foo", string) :*
          field("bar", int).optional :*
          field("animal", Animal.json)
      )

      val result = CirceJson.obj(
        "type" := "object",
        "properties" := List(
          "foo" := CirceJson.obj(
            "type" := "string"
          ),
          "bar" := CirceJson.obj(
            "type" := "integer"
          ),
          "animal" := CirceJson.obj(
            "type" := "string",
            "enum" := List("bird", "cat", "dog")
          )
        ),
        "required" := List("foo", "animal")
      )

      assertTrue(input == result)
  )
