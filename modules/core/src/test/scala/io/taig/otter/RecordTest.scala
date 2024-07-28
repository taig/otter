package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import munit.FunSuite
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

final class RecordTest extends FunSuite:
  test("encode"):
    val codec = record(field("foo", string) :* field("bar", int))

    assertEquals(
      obtained = codec.encode(("foobar", 42)),
      expected = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))
    )

  test("encode: optional"):
    val codec = record(field("foo", string) :* field("bar", int)).optional

    assertEquals(
      obtained = codec.encode(("foobar", 42).some),
      expected = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))
    )

    assertEquals(obtained = codec.encode(none), expected = Data.Null)

  test("encode: optional (product)"):
    val codec =
      record(field("a", string) :* field("b", int)).optional.zip(record(field("c", string) :* field("d", int)).optional)

    assertEquals(
      obtained = codec.encode(("foobar", 42).some, ("foobar", 42).some),
      expected = Data.Object
        .of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.String("foobar"), "d" -> Data.Number(42))
    )

    assertEquals(
      obtained = codec.encode(none, ("foobar", 42).some),
      expected =
        Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.String("foobar"), "d" -> Data.Number(42))
    )

    assertEquals(
      obtained = codec.encode(("foobar", 42).some, none),
      expected =
        Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Null, "d" -> Data.Null)
    )

    assertEquals(
      obtained = codec.encode(none, none),
      expected = Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.Null, "d" -> Data.Null)
    )
