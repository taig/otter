package io.taig.otter.codec

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment
import io.taig.otter.component.JsonComponent.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import cats.syntax.all.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object JsonSchemaRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaEncoderTest")(
    test("sample"):
      val input = JsonSchemaRenderer.render(string)
      val result = CirceJson.obj(
        "type" := "string"
      )

      assertTrue(input == result)
  )
