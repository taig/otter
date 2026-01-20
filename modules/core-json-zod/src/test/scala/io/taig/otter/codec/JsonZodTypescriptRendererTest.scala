package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Json
import io.taig.otter.fixture
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*
import zio.test.ZIOSpecDefault
import cats.syntax.all.*

object JsonZodTypescriptRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonZodTypescriptRendererTest")(
    test("object"):
      val schema = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", fixture.json.animal.optional)

      val obtained = JsonZodTypescriptRenderer.render(schema).mkString("\n\n")

      val expected = """z.object({
                       |  "name": z.string(),
                       |  "age": z.optional(z.number()),
                       |  "gender": z.literal("unknown"),
                       |  "pet": z.nullable(
                       |    z.enum(
                       |      [
                       |        "bird",
                       |        "cat",
                       |        "dog"
                       |      ]
                       |    )
                       |  )
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("object: name"):
      val name = (
        field("first", string) :*
          field("last", string)
      ).attr(Keys.name, "Name")

      val schema = field("name", name) :* field("age", int).optional

      val obtained = JsonZodTypescriptRenderer.render(schema).mkString("\n\n")

      val expected = """const Name = z.object({
                       |  "first": z.string(),
                       |  "last": z.string()
                       |});
                       |
                       |z.object({
                       |  "name": Name,
                       |  "age": z.optional(z.number())
                       |})""".stripMargin

      assertTrue(obtained == expected)
  )
