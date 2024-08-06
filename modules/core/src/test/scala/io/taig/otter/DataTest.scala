package io.taig.otter

import munit.FunSuite

final class DataTest extends FunSuite:
  test("print: primitive"):
    assertEquals(
      obtained = Data.String("foobar").print,
      expected = "\"foobar\""
    )

    assertEquals(
      obtained = Data.Boolean(true).print,
      expected = "true"
    )

    assertEquals(
      obtained = Data.Boolean(false).print,
      expected = "false"
    )

    assertEquals(
      obtained = Data.Number(0).print,
      expected = "0"
    )

    assertEquals(
      obtained = Data.Number(1.23f).print,
      expected = "1.23"
    )

    assertEquals(
      obtained = Data.Number(1.23d).print,
      expected = "1.23"
    )

  test("print: null"):
    assertEquals(
      obtained = Data.Null.print,
      expected = "null"
    )

  test("print: object"):
    assertEquals(
      obtained = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42)).print,
      expected = "{foo:\"foobar\",bar:42}"
    )

  test("print: array"):
    assertEquals(
      obtained = Data.Array.of(Data.String("foobar"), Data.Number(42)).print,
      expected = "[\"foobar\",42]"
    )
