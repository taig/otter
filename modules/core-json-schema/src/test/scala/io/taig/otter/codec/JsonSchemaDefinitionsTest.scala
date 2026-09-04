package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** Naming, hoisting and the cycle a name breaks.
  *
  * The fixture `json.tree` carries no name, so it is not rendered here at all: an anonymous cycle does not terminate,
  * and there is nothing a renderer can do about that. `Tree` below is the same schema with a name on the `lazy val`
  * that is reached again, which is the documented way to write one down.
  */
object JsonSchemaDefinitionsTest extends ZIOSpecDefault:
  private def render(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(schema).value.noSpaces

  private lazy val tree: Json.Record[Tree] =
    (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")

  private val Dialect = """"$schema":"https://json-schema.org/draft/2020-12/schema""""

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaDefinitionsTest")(
    test("a name on the root is not a reason to point at the root from itself"):
      assertTrue(
        render(json.book.attr(Keys.name, "Book")) == render(json.book)
      )
    ,
    test("a schema that refers to itself keeps the definition it has to refer to"):
      assertTrue(
        render(tree) ==
          s"""{$Dialect,"$$ref":"#/$$defs/Tree","$$defs":{"Tree":{"type":"object","properties":""" +
          """{"value":{"type":"integer"},"children":{"type":"array","items":{"$ref":"#/$defs/Tree"}}},""" +
          """"required":["value","children"]}}}"""
      )
    ,
    test("a name reached twice is declared once"):
      val schema = (field("left", tree) :* field("right", tree)).toRecord

      assertTrue(
        render(schema) ==
          s"""{$Dialect,"type":"object","properties":{"left":{"$$ref":"#/$$defs/Tree"},""" +
          """"right":{"$ref":"#/$defs/Tree"}},"required":["left","right"],""" +
          """"$defs":{"Tree":{"type":"object","properties":{"value":{"type":"integer"},""" +
          """"children":{"type":"array","items":{"$ref":"#/$defs/Tree"}}},"required":["value","children"]}}}"""
      )
    ,
    test("a name a pointer cannot hold verbatim is escaped"):
      assertTrue(
        render(field("book", json.book.attr(Keys.name, "a/b~c")).toRecord).contains(""""$ref":"#/$defs/a~1b~0c"""")
      )
    ,
    test("a profile that will not follow a reference inlines instead, and says so where it cannot"):
      val inlining = JsonSchemaProfile.Draft202012.copy(definitions = None)

      val named = JsonSchemaRenderer.writer(inlining).render(json.book.attr(Keys.name, "Book"))
      val recursive = JsonSchemaRenderer.writer(inlining).render(tree)

      assertTrue(
        named.issues.isEmpty,
        !named.value.noSpaces.contains("$ref"),
        recursive.issues == List(JsonSchemaIssue.Recursive(Some("Tree"), "Tree"))
      )
    ,
    test("a name does not survive the composition that rebuilds a node"):
      val named = json.book.attr(Keys.name, "Book")

      assertTrue(
        Json.name(io.taig.otter.JsonSchema.Namespaces, named).contains("Book"),
        Json.name(io.taig.otter.JsonSchema.Namespaces, (field("a", int) :* field("b", int)).toRecord).isEmpty
      )
  )
