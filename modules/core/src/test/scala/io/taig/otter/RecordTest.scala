package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import munit.FunSuite
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

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
          Violations.namespaceNec(
            History.Step.Field("foo"),
            Violation(Constraint.Type("string"), actual = Data.String("array"))
          ),
          Violations.namespaceNec(
            History.Step.Field("bar"),
            Violation(Constraint.Type("number"), actual = Data.String("string"))
          )
        )
        .invalid
    )

  test("decode: optional"):
    val codec = record(field("foo", string) :* field("bar", int)).optional

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
    val codec = record(field("a", string) :* field("b", int)).optional
      .zip(record(field("c", string) :* field("d", int)).optional)

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
    val codec = record(field("foo", string.optional) :* field("bar", int.optional)).optional

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
    val codec = record(field("foo", string) :* field("bar", int)).optional

    assertEquals(
      obtained = codec.encode(("foobar", 42).some),
      expected = Data.Object.of("foo" -> Data.String("foobar"), "bar" -> Data.Number(42))
    )

    assertEquals(obtained = codec.encode(none), expected = Data.Null)

  test("encode: optional (product)"):
    val codec = record(field("a", string) :* field("b", int)).optional
      .zip(record(field("c", string) :* field("d", int)).optional)

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
      field("foo", string.optional).modifyMetadata(_.put(nulls, Null.Hide)) :*
        field("bar", int.optional).modifyMetadata(_.put(nulls, Null.Show)) :*
        field("baz", long.optional)
    )

    assertEquals(
      obtained = codec.modifyMetadata(_.put(nulls, Null.Show)).encode((none, none, none)),
      expected = Data.Object.of("bar" -> Data.Null, "baz" -> Data.Null)
    )

    assertEquals(
      obtained = codec.modifyMetadata(_.put(nulls, Null.Hide)).encode((none, none, none)),
      expected = Data.Object.of("bar" -> Data.Null)
    )
