package io.taig.otter

import cats.syntax.all.*
import munit.FunSuite

final class DataTest extends FunSuite:
  test("show: primitive"):
    assertEquals(
      obtained = Data.String("foobar").show,
      expected = "\"foobar\""
    )

    assertEquals(
      obtained = Data.Boolean(true).show,
      expected = "true"
    )

    assertEquals(
      obtained = Data.Boolean(false).show,
      expected = "false"
    )

    assertEquals(
      obtained = Data.Number(0).show,
      expected = "0"
    )

    assertEquals(
      obtained = Data.Number(1.23f).show,
      expected = 1.23f.show
    )

    assertEquals(
      obtained = Data.Number(1.23d).show,
      expected = 1.23d.show
    )

  test("show: null"):
    assertEquals(
      obtained = Data.Null.show,
      expected = "null"
    )

  test("show: object"):
    assertEquals(
      obtained = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42)).show,
      expected = "{\"foo\":\"foobar\",\"bar\":42}"
    )

  test("show: array"):
    assertEquals(
      obtained = Data.Array.of(Data.String("foobar"), Data.Number(42)).show,
      expected = "[\"foobar\",42]"
    )
