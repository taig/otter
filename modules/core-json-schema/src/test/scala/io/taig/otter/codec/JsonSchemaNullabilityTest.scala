package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** The three ways a document says a value may also be nothing.
  *
  * One question with three answers, and which one a profile gives is the only thing that varies here, so every case
  * renders the same schema and the assertions are the diff. The last test is the one that says why there are three
  * rather than two: a value that is not a bare `type` is something [[JsonSchemaProfile.Nullability.TypeArray]] cannot
  * rewrite, so it widens to the alternation and null joins the value's own alternatives -- and a consumer whose dialect
  * has no `null` type at all is left with neither. That is what the keyword is for, and it is the only one of the three
  * that leaves what the value is alone.
  */
object JsonSchemaNullabilityTest extends ZIOSpecDefault:
  private def render(nullability: JsonSchemaProfile.Nullability)(schema: Json.Node[?, ?]): CirceJson =
    JsonSchemaRenderer
      .writer(JsonSchemaProfile.Draft202012.copy(dialect = None, nullability = nullability))
      .render(schema)
      .value

  private val anyOf = render(JsonSchemaProfile.Nullability.AnyOf)

  private val typeArray = render(JsonSchemaProfile.Nullability.TypeArray)

  private val flag = render(JsonSchemaProfile.Nullability.Flag)

  /** The union's alternatives, as the document ended up stating them. */
  private def alternatives(document: CirceJson, property: String): List[CirceJson] =
    document.hcursor.downField("properties").downField(property).downField("anyOf").values.toList.flatten.toList

  private val shape: Json.Record[Option[Shape]] = field("shape", json.shape).optional.nullable.toRecord

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaNullabilityTest")(
    test("an alternation names null beside what the value otherwise is"):
      assertTrue(
        anyOf(json.nullableTag).noSpaces ==
          """{"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["title","tag"]}"""
      )
    ,
    test("a type array says it in the keyword the value already has"):
      assertTrue(
        typeArray(json.nullableTag).noSpaces ==
          """{"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"type":["integer","null"]}},"required":["title","tag"]}"""
      )
    ,
    test("the flag says it beside the value, and says nothing about the value itself"):
      assertTrue(
        flag(json.nullableTag).noSpaces ==
          """{"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"type":"integer","nullable":true}},"required":["title","tag"]}"""
      )
    ,
    test("a value that is not a bare type widens the type array back to the alternation"):
      assertTrue(typeArray(shape) == anyOf(shape))
    ,
    test("the alternation makes null a branch of the union, and the flag leaves the branches alone"):
      assertTrue(
        alternatives(anyOf(shape), "shape").length == 4,
        alternatives(anyOf(shape), "shape").lastOption.map(_.noSpaces).contains("""{"type":"null"}"""),
        alternatives(flag(shape), "shape").length == 3,
        alternatives(flag(shape), "shape") == alternatives(anyOf(shape), "shape").init
      )
  )
