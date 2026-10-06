package io.taig.otter.codec

import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object JsonTypescriptDynamicTest extends ZIOSpecDefault:
  private def render(schema: io.taig.otter.Json.Node[?, ?]): String =
    JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n\n")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptDynamicTest")(
    test("dynamic variants use Effect unknown, record, and array schemas"):
      assertTrue(
        render(dynamic.any) == "Schema.Unknown",
        render(dynamic.obj) == "Schema.Record(Schema.String, Schema.Unknown)",
        render(dynamic.array) == "Schema.Array(Schema.Unknown)"
      )
  )
