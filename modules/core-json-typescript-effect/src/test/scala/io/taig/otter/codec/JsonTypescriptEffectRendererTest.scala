package io.taig.otter.codec

import io.taig.otter.fixture
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*
import zio.test.ZIOSpecDefault

object JsonTypescriptEffectRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptEffectRendererTest")(
    test("Json.Constant"):
      val schema = constant(string, "foobar")
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Literal("foobar")"""
      assertTrue(obtained == expected)
    ,
    test("Json.Collection"):
      val schema = collection.list(int)
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Array(Schema.Number)"""
      assertTrue(obtained == expected)
    ,
    test("Json.Dictionary"):
      val schema = dictionary.list(boolean)
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Record({
                       |  "key": Schema.String,
                       |  "value": Schema.Boolean
                       |})""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Json.Enumeration"):
      val obtained = JsonTypescriptEffectRenderer.render(fixture.json.animal).mkString("\n\n")
      val expected = """Schema.Literal(
                       |  "bird",
                       |  "cat",
                       |  "dog"
                       |)""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Json.Optional"):
      val schema = string.optional
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.NullOr(Schema.String)"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: boolean"):
      val obtained = JsonTypescriptEffectRenderer.render(boolean).mkString("\n\n")
      val expected = """Schema.Boolean"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: coerce boolean"):
      val obtained = JsonTypescriptEffectRenderer.render(coerce(boolean)).mkString("\n\n")
      val expected = """CoerceBoolean"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: number"):
      val obtained = JsonTypescriptEffectRenderer.render(float).mkString("\n\n")
      val expected = """Schema.Number"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: text"):
      val obtained = JsonTypescriptEffectRenderer.render(string).mkString("\n\n")
      val expected = """Schema.String"""
      assertTrue(obtained == expected)
    ,
    test("Json.Record"):
      val schema = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", fixture.json.animal.optional)

      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")

      val expected = """Schema.Struct({
                       |  "name": Schema.String,
                       |  "age": Schema.optional(Schema.Number),
                       |  "gender": Schema.Literal("unknown"),
                       |  "pet": Schema.NullOr(
                       |    Schema.Literal(
                       |      "bird",
                       |      "cat",
                       |      "dog"
                       |    )
                       |  )
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("Json.Tuple"):
      val schema = string :* int :* fixture.json.animal.optional
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Tuple(
                       |  Schema.String,
                       |  Schema.Number,
                       |  Schema.NullOr(
                       |    Schema.Literal(
                       |      "bird",
                       |      "cat",
                       |      "dog"
                       |    )
                       |  )
                       |)""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Union"):
      val schema = branch("foo", string) :+
        branch("bar", int) :+
        branch("foobar", fixture.json.animal)

      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Union(
                       |  Schema.String,
                       |  Schema.Number,
                       |  Schema.Literal(
                       |    "bird",
                       |    "cat",
                       |    "dog"
                       |  )
                       |)""".stripMargin
      assertTrue(obtained == expected)
  )
