package io.taig.openapi.schema

import cats.syntax.all.*
import munit.FunSuite

final class TypeTest extends FunSuite:
  test("parse: bigDecimal") {
    assertEquals(
      obtained = Type.BigDecimal.parse("0"),
      expected = BigDecimal(0).some
    )
    assertEquals(
      obtained = Type.BigDecimal.parse("1.234"),
      expected = BigDecimal("1.234").some
    )
    assertEquals(
      obtained = Type.BigDecimal.parse(Long.MaxValue.toString),
      expected = BigDecimal(Long.MaxValue).some
    )
    assertEquals(
      obtained = Type.BigDecimal.parse("foobar"),
      expected = none
    )
  }

  test("parse: bigInt") {
    assertEquals(
      obtained = Type.BigInt.parse("0"),
      expected = BigInt(0).some
    )
    assertEquals(
      obtained = Type.BigInt.parse(Long.MaxValue.toString),
      expected = BigInt(Long.MaxValue).some
    )
    assertEquals(
      obtained = Type.BigInt.parse("foobar"),
      expected = none
    )
  }

  test("parse: boolean") {
    assertEquals(
      obtained = Type.Boolean.parse("true"),
      expected = true.some
    )
    assertEquals(
      obtained = Type.Boolean.parse("false"),
      expected = false.some
    )
    assertEquals(
      obtained = Type.Boolean.parse("foobar"),
      expected = none
    )
  }

  test("parse: double") {
    assertEquals(
      obtained = Type.Double.parse("0"),
      expected = 0d.some
    )
    assertEquals(
      obtained = Type.Double.parse("1.234"),
      expected = 1.234d.some
    )
    assertEquals(
      obtained = Type.Double.parse(Long.MaxValue.toString),
      expected = Long.MaxValue.toDouble.some
    )
    assertEquals(
      obtained = Type.Double.parse("foobar"),
      expected = none
    )
  }

  test("parse: float") {
    assertEquals(
      obtained = Type.Float.parse("0"),
      expected = 0f.some
    )
    assertEquals(
      obtained = Type.Float.parse("1.234"),
      expected = 1.234f.some
    )
    assertEquals(
      obtained = Type.Float.parse(Long.MaxValue.toString),
      expected = Long.MaxValue.toFloat.some
    )
    assertEquals(
      obtained = Type.Float.parse("foobar"),
      expected = none
    )
  }

  test("parse: int") {
    assertEquals(
      obtained = Type.Int.parse("0"),
      expected = 0.some
    )
    assertEquals(
      obtained = Type.Int.parse(Long.MaxValue.toString),
      expected = none
    )
    assertEquals(
      obtained = Type.Int.parse("foobar"),
      expected = none
    )
  }

  test("parse: long") {
    assertEquals(
      obtained = Type.Long.parse("0"),
      expected = 0L.some
    )
    assertEquals(
      obtained = Type.Long.parse(Long.MaxValue.toString),
      expected = Long.MaxValue.some
    )
    assertEquals(
      obtained = Type.Long.parse("foobar"),
      expected = none
    )
  }

  test("parse: string") {
    assertEquals(
      obtained = Type.String.parse("foobar"),
      expected = "foobar".some
    )
  }

  test("render: bigDecimal") {
    assertEquals(
      obtained = Type.BigDecimal.render(BigDecimal(0)),
      expected = "0"
    )
    assertEquals(
      obtained = Type.BigDecimal.render(BigDecimal("1.234")),
      expected = "1.234"
    )
    assertEquals(
      obtained = Type.BigDecimal.render(BigDecimal(Long.MaxValue)),
      expected = Long.MaxValue.toString
    )
  }

  test("render: bigInt") {
    assertEquals(
      obtained = Type.BigInt.render(BigInt(0)),
      expected = "0"
    )
    assertEquals(
      obtained = Type.BigInt.render(BigInt(Long.MaxValue)),
      expected = Long.MaxValue.toString
    )
  }

  test("render: boolean") {
    assertEquals(
      obtained = Type.Boolean.render(true),
      expected = "true"
    )
    assertEquals(
      obtained = Type.Boolean.render(false),
      expected = "false"
    )
  }

  test("render: double") {
    assertEquals(
      obtained = Type.Double.render(0d),
      expected = "0.0"
    )
    assertEquals(
      obtained = Type.Double.render(1.234d),
      expected = "1.234"
    )
  }

  test("render: float") {
    assertEquals(
      obtained = Type.Float.render(0f),
      expected = "0.0"
    )
    assertEquals(
      obtained = Type.Float.render(1.234f),
      expected = "1.234"
    )
  }

  test("render: int") {
    assertEquals(
      obtained = Type.Int.render(0),
      expected = "0"
    )
  }

  test("render: long") {
    assertEquals(
      obtained = Type.Long.render(0),
      expected = "0"
    )
    assertEquals(
      obtained = Type.Long.render(Long.MaxValue),
      expected = Long.MaxValue.toString
    )
  }

  test("render: string") {
    assertEquals(
      obtained = Type.String.render("foobar"),
      expected = "foobar"
    )
  }
