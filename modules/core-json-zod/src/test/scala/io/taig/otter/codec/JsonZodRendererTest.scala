package io.taig.otter.codec

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.JsonSyntax.*

object JsonZodRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonZodRendererTest")(
    test("default"):
      val input = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown"))

      val output = s"""z.object({
                      |"name": z.string(),
                      |"age": z.optional(z.number()),
                      |"gender": z.literal("unknown")
                      |})""".stripMargin

      assertTrue(JsonZodRenderer.render(input) == output)
  )
