package io.taig.otter.codec

import io.taig.data.Data
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.std
import zio.Scope
import zio.test.*

object JsonSchemaDynamicTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaDynamicTest")(
    test("dynamic variants render permissive schemas and object bounds"):
      val validation = std.obj.minimum[List[(String, Data)]](2).contramap[Data.Object[Data]](_.values)
      val renderer = JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012)

      assertTrue(
        renderer.render(dynamic.any).value.noSpaces ==
          """{"$schema":"https://json-schema.org/draft/2020-12/schema"}""",
        renderer.render(dynamic.obj).value.noSpaces ==
          """{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object"}""",
        renderer.render(dynamic.array).value.noSpaces ==
          """{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"array","items":{}}""",
        renderer.render(dynamic.obj(validation)).value.noSpaces ==
          """{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","minProperties":2}"""
      )
    ,
    test("strict profile reports unrestricted dynamic content"):
      val renderer = JsonSchemaRenderer.writer(JsonSchemaProfile.Strict)
      val any = renderer.render(dynamic.any)
      val array = renderer.render(dynamic.array)

      assertTrue(
        any.value.noSpaces == "{}",
        any.issues == List(JsonSchemaIssue.Open(None)),
        array.value.noSpaces == """{"type":"array","items":{}}""",
        array.issues == List(JsonSchemaIssue.Open(None))
      )
  )
