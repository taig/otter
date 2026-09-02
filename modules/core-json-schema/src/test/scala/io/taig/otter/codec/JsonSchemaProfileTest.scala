package io.taig.otter.codec

import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonSchemaDocument
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import io.taig.validation.std
import zio.Scope
import zio.test.*

/** What a strict structured output consumer is told, and what it is not.
  *
  * Everything the profile cannot say it leaves out and records. The recording is half the point: a caller that hands a
  * document to a consumer which will reject anything it did not ask for wants to know that the schema said more than
  * the document does, and a caller generating documentation does not.
  */
object JsonSchemaProfileTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): JsonSchemaDocument =
    JsonSchemaRenderer.reader(JsonSchemaProfile.Strict).render(schema)

  private lazy val tree: Json.Record[Tree] =
    (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaProfileTest")(
    test("every object closes, every key is listed, and no dialect is declared"):
      val document = read(json.book)

      assertTrue(
        document.issues.isEmpty,
        document.value.noSpaces ==
          """{"type":"object","properties":{"title":{"type":"string"},"pages":{"type":"integer"},""" +
          """"read":{"type":"boolean"}},"required":["title","pages","read"],"additionalProperties":false}"""
      )
    ,
    test("a key that may be absent is listed anyway and allowed to hold nothing instead"):
      val document = read(json.omittedTag)

      assertTrue(
        document.issues.isEmpty,
        document.value.noSpaces ==
          """{"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["title","tag"],""" +
          """"additionalProperties":false}"""
      )
    ,
    test("a strict field that drops its key is the one narrowing that is not safe, and it is reported"):
      assertTrue(read(json.nestedTag).issues == List(JsonSchemaIssue.Total(None, "tag")))
    ,
    test("a constraint the consumer will not read is dropped, and the decoder still enforces it"):
      val document = read(string(std.text.minimum[String](1) & std.text.maximum[String](64)))

      assertTrue(
        document.value.noSpaces == """{"type":"string"}""",
        document.issues == List(
          JsonSchemaIssue.Dropped(None, Constraint.Primitive.Text.Minimum(io.taig.validation.Comparison(1L, false))),
          JsonSchemaIssue.Dropped(None, Constraint.Primitive.Text.Maximum(io.taig.validation.Comparison(64L, false)))
        )
      )
    ,
    test("a registered format survives and an invented one does not"):
      assertTrue(
        read(uuid).value.noSpaces == """{"type":"string","format":"uuid"}""",
        read(uuid).issues.isEmpty,
        read(json.counter).value.noSpaces == """{"type":"string"}""",
        read(json.counter).issues == List(JsonSchemaIssue.Format(None, "int"))
      )
    ,
    test("a dictionary cannot be said at all, because every object has to close"):
      val document = read(json.editions)

      assertTrue(
        document.value.noSpaces == """{"type":"object"}""",
        document.issues == List(JsonSchemaIssue.Open(None))
      )
    ,
    test("a tuple widens to a homogeneous array, which is the one place the document admits more than the decoder"):
      val document = read(TNil :* string :* int)

      assertTrue(
        document.value.noSpaces ==
          """{"type":"array","items":{"anyOf":[{"type":"string"},{"type":"integer"}]}}""",
        document.issues == List(JsonSchemaIssue.Positional(None))
      )
    ,
    test("a coercion says the one form it writes, because a producer writing to order has no reason to be lax"):
      val document = read(coerce(int))

      assertTrue(
        document.value.noSpaces == """{"type":"integer"}""",
        document.issues == List(JsonSchemaIssue.Coerced(None))
      )
    ,
    test("a schema that refers to itself is still rendered, and refused"):
      val document = read(tree)

      assertTrue(
        document.value.noSpaces.contains(""""$ref":"#/$defs/Tree""""),
        document.issues == List(JsonSchemaIssue.Recursive(Some("Tree"), "Tree")),
        document.toEither.isLeft
      )
    ,
    test("a document the profile could say everything of comes back as a right"):
      assertTrue(read(json.book).toEither.isRight)
  )
