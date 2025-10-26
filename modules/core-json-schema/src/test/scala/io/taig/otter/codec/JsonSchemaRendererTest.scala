package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.JsonSyntax.*
import zio.*
import zio.test.*

object JsonSchemaRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaEncoderTest")(
    test("sample"):
      val input = JsonSchemaRenderer.render(
        field("foo", string) :*
          field("bar", int).optional
      )

      val result = CirceJson.obj(
        "type" := "object",
        "properties" := List(
          "foo" := CirceJson.obj(
            "type" := "string"
          ),
          "bar" := CirceJson.obj(
            "type" := "integer"
          )
        ),
        "required" := List("foo")
      )

      assertTrue(input == result)
  )
