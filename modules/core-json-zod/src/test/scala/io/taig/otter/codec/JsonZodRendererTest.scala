package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*
import zio.test.ZIOSpecDefault
import scala.collection.immutable.ListMap
import cats.syntax.all.*

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
      val schema = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", Animal.json.optional)

      val obtained = JsonZodExpressionRenderer.render(schema).runA(ListMap.empty).value.render

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
  )
