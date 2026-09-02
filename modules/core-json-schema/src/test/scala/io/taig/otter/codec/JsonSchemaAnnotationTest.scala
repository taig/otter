package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.syntax.JsonSchemaSyntax.*
import zio.Scope
import zio.test.*

/** What a schema says about itself.
  *
  * `Keys.title` and `Keys.description` have been declared since before there was an interpreter to read them; this is
  * that interpreter.
  */
object JsonSchemaAnnotationTest extends ZIOSpecDefault:
  private def render(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer
      .writer(JsonSchemaProfile.Draft202012)
      .render(schema)
      .value
      .mapObject(_.remove("$schema"))
      .noSpaces

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaAnnotationTest")(
    test("a label leads and a default trails, which is where a reader looks for them"):
      assertTrue(
        render(int.title("Pages").description("How long the book is").default(0)) ==
          """{"title":"Pages","description":"How long the book is","type":"integer","default":0}"""
      )
    ,
    test("examples and deprecation are said the same way"):
      assertTrue(
        render(string.examples("Dune", "Emma").deprecated) ==
          """{"type":"string","examples":["Dune","Emma"],"deprecated":true}"""
      )
    ,
    test("what is said to JSON Schema wins over what is said to JSON, which wins over what is said to everything"):
      val schema = string
        .attr(Keys.description, "everything")
        .attr(Json.Namespace, Keys.description, "json")
        .attr(JsonSchema.Namespace, Keys.description, "json schema")

      assertTrue(
        render(schema) == """{"description":"json schema","type":"string"}""",
        render(string.attr(Keys.description, "everything").attr(Json.Namespace, Keys.description, "json")) ==
          """{"description":"json","type":"string"}""",
        render(string.attr(Metadata.Namespace.Global, Keys.description, "everything")) ==
          """{"description":"everything","type":"string"}"""
      )
    ,
    test("a field describes the property and what it holds describes the type, and the field is the more specific"):
      assertTrue(
        render(field("pages", int.description("a number")).toRecord) ==
          """{"type":"object","properties":{"pages":{"description":"a number","type":"integer"}},""" +
          """"required":["pages"]}""",
        render(field("pages", int.description("a number")).description("how long").toRecord) ==
          """{"type":"object","properties":{"pages":{"description":"how long","type":"integer"}},""" +
          """"required":["pages"]}"""
      )
    ,
    test("keywords merge over what was derived"):
      assertTrue(
        render(string.keywords(CirceJson.obj("x-order" -> CirceJson.fromInt(1)))) ==
          """{"type":"string","x-order":1}""",
        render(collection.list(int).keywords(CirceJson.obj("items" -> CirceJson.obj()))) ==
          """{"type":"array","items":{}}"""
      )
    ,
    test("a schema replaces what was derived, and still carries its own label"):
      assertTrue(
        render(string.schema(CirceJson.obj("type" -> CirceJson.fromString("integer"))).title("Overridden")) ==
          """{"title":"Overridden","type":"integer"}"""
      )
  )
