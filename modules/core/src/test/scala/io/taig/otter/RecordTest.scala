package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import munit.FunSuite

final class RecordTest extends FunSuite:
  test("decode"):
    val codec = record(field("foo", string) :* field("bar", int))

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))),
      expected = ("foobar", 42).valid
    )

    assertEquals(
      obtained = codec.decode(Data.Null),
      expected = Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))).invalid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.Array.Empty, "bar" -> Data.String("foobar"))),
      expected = Violations
        .of(
          Step.Field("foo") -> Violation(Constraint.Type("string"), actual = Data.String("array")),
          Step.Field("bar") -> Violation(Constraint.Type("int"), actual = Data.String("string"))
        )
        .invalid
    )

  test("decode: optional"):
    val codec = record(field("foo", string) :* field("bar", int)).nullable

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))),
      expected = ("foobar", 42).some.valid
    )

    assertEquals(obtained = codec.decode(Data.Null), expected = none.valid)

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.Null, "bar" -> Data.Null)),
      expected = none.valid
    )

  test("decode: optional (product)"):
    val codec = record(field("a", string) :* field("b", int)).nullable
      .zip(record(field("c", string) :* field("d", int)).nullable)

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          "a" -> Data.String("foobar"),
          "b" -> Data.Number(42),
          "c" -> Data.String("foobar"),
          "d" -> Data.Number(42)
        )
      ),
      expected = (("foobar", 42).some, ("foobar", 42).some).valid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.String("foobar"), "d" -> Data.Number(42))
      ),
      expected = (none, ("foobar", 42).some).valid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of("a" -> Data.Null, "c" -> Data.String("foobar"), "d" -> Data.Number(42))
      ),
      expected = (none, ("foobar", 42).some).valid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("c" -> Data.String("foobar"), "d" -> Data.Number(42))),
      expected = (none, ("foobar", 42).some).valid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Null, "d" -> Data.Null)
      ),
      expected = (("foobar", 42).some, none).valid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Null)),
      expected = (("foobar", 42).some, none).valid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42))),
      expected = (("foobar", 42).some, none).valid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.Null, "d" -> Data.Null)),
      expected = (none, none).valid
    )

    assertEquals(obtained = codec.decode(Data.Object.Empty), expected = (none, none).valid)

  test("decode: optional (nested)"):
    val codec = record(field("foo", string.nullable) :* field("bar", int.nullable)).nullable

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))),
      expected = ("foobar".some, 42.some).some.valid
    )

    assertEquals(
      obtained = codec.decode(Data.Object.of("foo" -> Data.Null, "bar" -> Data.Null)),
      expected = none.valid
    )

    assertEquals(obtained = codec.decode(Data.Object.Empty), expected = none.valid)

    assertEquals(obtained = codec.decode(Data.Null), expected = none.valid)

  test("encode"):
    val codec = record(field("foo", string) :* field("bar", int))

    assertEquals(
      obtained = codec.encode(("foobar", 42)),
      expected = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))
    )

  test("encode: optional"):
    val codec = record(field("foo", string) :* field("bar", int)).nullable

    assertEquals(
      obtained = codec.encode(("foobar", 42).some),
      expected = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))
    )

    assertEquals(obtained = codec.encode(none), expected = Data.Null)

  test("encode: optional (product)"):
    val codec = record(field("a", string) :* field("b", int)).nullable
      .zip(record(field("c", string) :* field("d", int)).nullable)

    assertEquals(
      obtained = codec.encode((("foobar", 42).some, ("foobar", 42).some)),
      expected = Data.Object.of(
        "a" -> Data.String("foobar"),
        "b" -> Data.Number(42),
        "c" -> Data.String("foobar"),
        "d" -> Data.Number(42)
      )
    )

    assertEquals(
      obtained = codec.encode((none, ("foobar", 42).some)),
      expected =
        Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.String("foobar"), "d" -> Data.Number(42))
    )

    assertEquals(
      obtained = codec.encode((("foobar", 42).some, none)),
      expected =
        Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Null, "d" -> Data.Null)
    )

    assertEquals(
      obtained = codec.encode((none, none)),
      expected = Data.Object.of("a" -> Data.Null, "b" -> Data.Null, "c" -> Data.Null, "d" -> Data.Null)
    )

  test("encode: nulls"):
    val codec = record(
      field("foo", string.nullable).nulls(Null.Hide) :*
        field("bar", int.nullable).nulls(Null.Show) :*
        field("baz", long.nullable)
    )

    assertEquals(
      obtained = codec.nulls(Null.Show).encode((none, none, none)),
      expected = Data.Object.of("bar" -> Data.Null, "baz" -> Data.Null)
    )

    assertEquals(
      obtained = codec.nulls(Null.Hide).encode((none, none, none)),
      expected = Data.Object.of("bar" -> Data.Null)
    )
