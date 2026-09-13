package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.*
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.Comparison
import io.taig.validation.std
import zio.Scope
import zio.test.*

import java.util.regex.Pattern

object JsonSchemaFidelityTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): JsonSchemaDocument =
    JsonSchemaRenderer.reader(JsonSchemaProfile.Draft202012.copy(dialect = None)).render(schema)

  private def json(value: String): String = value

  private def bound(name: String, value: Int): CirceJson = CirceJson.obj(name -> CirceJson.fromInt(value))

  override val spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaFidelityTest")(
    test("repeated numeric bounds remain conjunctions in either order"):
      val first = read(
        double(std.number.minimum[Double](Comparison(10.0, false)) & std.number.minimum[Double](Comparison(0.0, false)))
      )
      val second = read(
        double(std.number.minimum[Double](Comparison(0.0, false)) & std.number.minimum[Double](Comparison(10.0, false)))
      )
      assertTrue(
        first.value.hcursor.downField("allOf").focus.contains(CirceJson.arr(bound("minimum", 10), bound("minimum", 0))),
        second.value.hcursor
          .downField("allOf")
          .focus
          .contains(CirceJson.arr(bound("minimum", 0), bound("minimum", 10))),
        first.issues.isEmpty,
        second.issues.isEmpty
      )
    ,
    test("contradictory constraints and repeated multiples are retained"):
      assertTrue(
        read(
          double(
            std.number.minimum[Double](Comparison(10.0, false)) & std.number.maximum[Double](Comparison(0.0, false))
          )
        ).value.noSpaces ==
          CirceJson
            .obj(
              "type" -> CirceJson.fromString("number"),
              "minimum" -> CirceJson.fromDoubleOrNull(10),
              "maximum" -> CirceJson.fromDoubleOrNull(0)
            )
            .noSpaces,
        read(int(std.number.multiple[Int](2) & std.number.multiple[Int](3))).value.noSpaces ==
          json(
            """{"type":"integer","minimum":-2147483648,"maximum":2147483647,"allOf":[{"multipleOf":2},{"multipleOf":3}]}"""
          )
      )
    ,
    test("collection and dictionary constraints do not overwrite each other"):
      val array = read(
        collection.list(string, std.collection.minimum[List[String]](10) & std.collection.minimum[List[String]](0))
      )
      val obj = read(
        dictionary.map(
          string,
          string,
          std.obj.minimum[Map[String, String]](10) & std.obj.minimum[Map[String, String]](0)
        )
      )
      assertTrue(
        array.value.hcursor
          .downField("allOf")
          .focus
          .map(_.noSpaces)
          .contains(json("""[{"minItems":10},{"minItems":0}]""")),
        obj.value.hcursor
          .downField("allOf")
          .focus
          .map(_.noSpaces)
          .contains(json("""[{"minProperties":10},{"minProperties":0}]"""))
      )
    ,
    test("count bounds remain valid schema keywords even when the constraint is impossible"):
      assertTrue(
        read(collection.list(string, std.collection.maximum[List[String]](Comparison(0L, true)))).value.noSpaces ==
          json("""{"type":"array","items":{"type":"string"},"not":{}}"""),
        read(
          collection.list(string, std.collection.minimum[List[String]](Comparison(Long.MaxValue, true)))
        ).value.noSpaces ==
          json("""{"type":"array","items":{"type":"string"},"minItems":9223372036854775808}""")
      )
    ,
    test("integer carrier bounds survive mappings and additional constraints"):
      assertTrue(
        read(int).value.noSpaces == json("""{"type":"integer","minimum":-2147483648,"maximum":2147483647}"""),
        read(long).value.noSpaces == json(
          """{"type":"integer","minimum":-9223372036854775808,"maximum":9223372036854775807}"""
        ),
        read(int.map(_.toString)).value.noSpaces == read(int).value.noSpaces,
        read(int(std.number.minimum(Comparison(10, false)))).value.noSpaces == json(
          """{"type":"integer","maximum":2147483647,"allOf":[{"minimum":-2147483648},{"minimum":10}]}"""
        )
      )
    ,
    test("UTF-16 lengths are omitted with their original constraints reported"):
      val minimum = Constraint.Primitive.Text.Minimum(Comparison(2L, false))
      val maximum = Constraint.Primitive.Text.Maximum(Comparison(3L, true))
      val document = read(string(std.text.minimum[String](2) & std.text.maximum[String](Comparison(3L, true))))
      assertTrue(
        document.value.noSpaces == json("""{"type":"string"}"""),
        document.issues == List(JsonSchemaIssue.Dropped(None, minimum), JsonSchemaIssue.Dropped(None, maximum))
      )
    ,
    test("regex flags and unsupported constructs are reported, not discarded from the pattern"):
      val patterns =
        List(Pattern.compile("abc", Pattern.CASE_INSENSITIVE), Pattern.compile("a++"), Pattern.compile("(?i)abc"))
      assertTrue(patterns.forall: pattern =>
        val document = read(string(std.text.matches[String](pattern)))
        document.value.noSpaces == json("""{"type":"string"}""") &&
        document.issues == List(JsonSchemaIssue.Dropped(None, Constraint.Primitive.Text.Matches(pattern))))
    ,
    test("two supported patterns both survive"):
      val document = read(
        string(std.text.matches[String](Pattern.compile("a.*")) & std.text.matches[String](Pattern.compile(".*b")))
      )
      assertTrue(document.value.hcursor.downField("allOf").values.exists(_.size == 2), document.issues.isEmpty)
    ,
    test("TypeArray falls back whenever another keyword can exclude null"):
      val nullable = JsonSchemaProfile.Nullability.TypeArray
      def field(name: String, value: String): (String, CirceJson) = name -> CirceJson.fromString(value)
      val schemas = List(
        CirceJson.obj(field("title", "Answer"), field("type", "string"), field("const", "yes")),
        CirceJson.obj(field("type", "string"), "enum" -> CirceJson.arr(CirceJson.fromString("yes"))),
        CirceJson.obj(field("type", "string"), field("$ref", "#/$defs/Answer")),
        CirceJson.obj(field("type", "string"), "allOf" -> CirceJson.arr(CirceJson.obj(field("const", "yes")))),
        CirceJson.obj(field("type", "string"), "not" -> CirceJson.obj(field("type", "null")))
      )
      assertTrue(
        schemas.forall(schema =>
          nullable(schema) == CirceJson.obj("anyOf" -> CirceJson.arr(schema, CirceJson.obj(field("type", "null"))))
        ),
        nullable(
          CirceJson.obj(field("title", "Count"), field("type", "integer"), "minimum" -> CirceJson.fromInt(0))
        ).noSpaces ==
          json("""{"title":"Count","type":["integer","null"],"minimum":0}""")
      )
    ,
    test("an optional constant uses a null alternative with TypeArray"):
      val document = JsonSchemaRenderer
        .reader(
          JsonSchemaProfile.Draft202012.copy(dialect = None, nullability = JsonSchemaProfile.Nullability.TypeArray)
        )
        .render(constant(string, "yes").optional)
      assertTrue(document.value.noSpaces == json("""{"anyOf":[{"type":"string","const":"yes"},{"type":"null"}]}"""))
  )
