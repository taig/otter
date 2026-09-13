package io.taig.otter.codec

import io.taig.otter.*
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.Comparison
import io.taig.validation.std
import zio.Scope
import zio.test.*

object JsonTypescriptNumericTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): String = JsonTypescriptEffectRenderer.reader.render(schema).mkString("\n")
  private def write(schema: Json.Node[?, ?]): String = JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n")

  override val spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptNumericTest")(
    test("Int bounds apply on both sides and survive mappings"):
      val expected = "Schema.Int.pipe(Schema.greaterThanOrEqualTo(-2147483648), Schema.lessThanOrEqualTo(2147483647))"
      assertTrue(read(int) == expected, write(int) == expected, read(int.map(_.toString)) == expected)
    ,
    test("Long excludes the rounded representation of Long.MaxValue plus one"):
      val expected =
        "Schema.Number.pipe(Schema.filter(Number.isInteger)).pipe(Schema.greaterThanOrEqualTo(-9223372036854775808), Schema.lessThan(9223372036854775808))"
      assertTrue(read(long) == expected, write(long) == expected)
    ,
    test("coercion checks the decimal text before converting to Number"):
      val source = read(coerce(int(std.number.minimum(Comparison(10, false)))))
      assertTrue(
        source.contains("export const CoerceInt ="),
        source.contains("Schema.pattern(RegExp("),
        source.contains("const significant ="),
        source.contains("const scale ="),
        source.contains("Schema.int()"),
        source.contains("Schema.greaterThanOrEqualTo(10)"),
        source.contains("Schema.lessThanOrEqualTo(2147483647)"),
        !source.contains("NumberFromString")
      )
    ,
    test("a user declaration cannot shadow the RegExp global or the Int coercion helper"):
      val source = read(
        field("named", string.attr(Keys.name, "RegExp")) :*
          field("helper", string.attr(Keys.name, "CoerceInt")) :* field("number", coerce(int))
      )
      assertTrue(source.contains("export const RegExp_2 ="), source.contains("export const CoerceInt_2 ="))
  )
