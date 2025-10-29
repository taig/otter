package io.taig.otter.codec

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Json

object JsonZodRendererTest extends ZIOSpecDefault:
  enum Animal:
    case Bird
    case Cat
    case Dog

  object Animal:
    val json: Json.Enumeration[Animal] = enumeration(string):
      case Bird => "bird"
      case Cat  => "cat"
      case Dog  => "dog"

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonZodRendererTest")(
    test("default"):
      val input = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", Animal.json.nullable)

      val output = s"""z.object({
                      |"name": z.string(),
                      |"age": z.optional(z.number()),
                      |"gender": z.literal("unknown"),
                      |"pet": z.nullable(z.enum(["bird", "cat", "dog"]))
                      |})""".stripMargin

      assertTrue(JsonZodRenderer.render(input) == output)
  )
