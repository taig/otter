package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Tree
import zio.Scope
import zio.test.*

object JsonSchemaNamingTest extends ZIOSpecDefault:
  private val renderer = JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaNamingTest")(
    test("different records requesting the same name keep their own definitions"):
      val left = field("a", int).toRecord.attr(Keys.name, "Shared")
      val right = field("b", string).toRecord.attr(Keys.name, "Shared")
      val schema = field("left", left) :* field("right", right) :* field("again", right)
      val document = renderer.render(schema)
      val cursor = document.value.hcursor

      assertTrue(
        document.issues.isEmpty,
        cursor.downField("properties").downField("left").get[String]("$ref") == Right("#/$defs/Shared"),
        cursor.downField("properties").downField("right").get[String]("$ref") == Right("#/$defs/Shared_2"),
        cursor.downField("properties").downField("again").get[String]("$ref") == Right("#/$defs/Shared_2"),
        cursor
          .downField("$defs")
          .downField("Shared")
          .downField("properties")
          .downField("a")
          .get[String]("type") == Right("integer"),
        cursor
          .downField("$defs")
          .downField("Shared_2")
          .downField("properties")
          .downField("b")
          .get[String]("type") == Right("string"),
        renderer.render(schema) == document
      )
    ,
    test("a child sharing its parent's name is not recursion"):
      val child = field("b", string).toRecord.attr(Keys.name, "Shared")
      val parent = field("child", child).toRecord.attr(Keys.name, "Shared")
      val document = renderer.render(parent)

      assertTrue(
        document.issues.isEmpty,
        document.value.hcursor.downField("properties").downField("child").get[String]("$ref") == Right(
          "#/$defs/Shared_2"
        ),
        document.value.hcursor
          .downField("$defs")
          .downField("Shared_2")
          .downField("properties")
          .downField("b")
          .get[String]("type") == Right("string")
      )
    ,
    test("inlining does not mistake a same-named child for recursion"):
      val child = field("b", string).toRecord.attr(Keys.name, "Shared")
      val parent = field("child", child).toRecord.attr(Keys.name, "Shared")
      val document = JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012.copy(definitions = None)).render(parent)

      assertTrue(
        document.issues.isEmpty,
        document.value.hcursor
          .downField("properties")
          .downField("child")
          .downField("properties")
          .downField("b")
          .get[String]("type") == Right("string")
      )
    ,
    test("allocated names skip names already requested explicitly"):
      val reserved = boolean.attr(Keys.name, "Shared_2")
      val first = int.attr(Keys.name, "Shared")
      val second = string.attr(Keys.name, "Shared")
      val document = renderer.render(field("reserved", reserved) :* field("first", first) :* field("second", second))

      assertTrue(
        document.value.hcursor.downField("properties").downField("second").get[String]("$ref") == Right(
          "#/$defs/Shared_3"
        ),
        document.value.hcursor.downField("$defs").downField("Shared_2").get[String]("type") == Right("boolean")
      )
    ,
    test("a recursive schema with a colliding name refers to its allocated definition"):
      val first = int.attr(Keys.name, "Tree")
      lazy val tree: Json.Record[Tree] =
        (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")
      val document = renderer.render(field("first", first) :* field("tree", tree))

      assertTrue(
        document.issues.isEmpty,
        document.value.hcursor
          .downField("$defs")
          .downField("Tree_2")
          .downField("properties")
          .downField("children")
          .downField("items")
          .get[String]("$ref") == Right("#/$defs/Tree_2")
      )
    ,
    test("separately constructed equal schemas have separate identities"):
      val first = int.attr(Keys.name, "Shared")
      val second = int.attr(Keys.name, "Shared")
      val document = renderer.render(field("first", first) :* field("second", second))

      assertTrue(
        document.value.hcursor.downField("properties").downField("second").get[String]("$ref") == Right(
          "#/$defs/Shared_2"
        )
      )
  )
