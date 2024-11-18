package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import cats.kernel.Eq

final class RecordTest extends OtterSuite:
  test("decode"):
    val codec = field("a", string) :* field("b", int)

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42))),
      expected = ("foobar", 42).valid
    )

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))).invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.Array.Empty, "b" -> Data.String("foobar"))),
      expected = Violations
        .of(
          Step.Field("a") -> Violation(Constraint.Type("string"), actual = Data.String("array")),
          Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("string"))
        )
        .invalid
    )

  test("decode: optional"):
    val codec = (field("a", string) :* field("b", int)).optional

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42))),
      expected = ("foobar", 42).some.valid
    )

    assertEq(obtained = codec.decode(Data.Object.Empty), expected = none.valid)

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations.rootNec(Violation.tpe("object", actual = "null")).invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"))),
      expected = Violations
        .of(Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("null")))
        .invalid
    )

  test("decode: optional (left)"):
    val codec: Record[(Option[(String, Int)], Float)] =
      (field("a", string) :* field("b", int)).optional :* field("c", float)

    assertEq(
      obtained = codec.decode(
        Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Number(1.0f))
      ),
      expected = (("foobar", 42).some, 1.0f).valid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("c" -> Data.Number(1.0f))),
      expected = (none, 1.0f).valid
    )

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations
        .rootNec(Violation(Constraint.Type("object"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.Empty),
      expected = Violations
        .of(Step.Field("c") -> Violation(Constraint.Type("float"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "c" -> Data.Number(1.0f))),
      expected = Violations
        .of(Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("null")))
        .invalid
    )

  test("decode: optional (right)"):
    val codec: Record[(String, Option[(Int, Float)])] =
      field("a", string) *: (field("b", int) :* field("c", float)).optional

    assertEq(
      obtained = codec.decode(
        Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42), "c" -> Data.Number(1.0f))
      ),
      expected = ("foobar", (42, 1.0f).some).valid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"))),
      expected = ("foobar", none).valid
    )

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations
        .rootNec(Violation(Constraint.Type("object"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.Empty),
      expected = Violations
        .of(Step.Field("a") -> Violation(Constraint.Type("string"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"), "c" -> Data.Number(1.0f))),
      expected = Violations
        .of(Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("null")))
        .invalid
    )

  test("decode: optional (left & right)"):
    val codec = (field("a", string) :* field("b", int)).optional
      .zip((field("c", string) :* field("d", int)).optional)

    assertEq(
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

    assertEq(
      obtained = codec.decode(
        Data.Object.of(
          "a" -> Data.String("foobar"),
          "b" -> Data.Number(42)
        )
      ),
      expected = (("foobar", 42).some, none).valid
    )

    assertEq(
      obtained = codec.decode(
        Data.Object.of(
          "c" -> Data.String("foobar"),
          "d" -> Data.Number(42)
        )
      ),
      expected = (none, ("foobar", 42).some).valid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("a" -> Data.String("foobar"))),
      expected = Violations
        .of(Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Object.of("c" -> Data.String("foobar"))),
      expected = Violations
        .of(Step.Field("d") -> Violation(Constraint.Type("int"), actual = Data.String("null")))
        .invalid
    )

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations
        .rootNec(Violation(Constraint.Type("object"), actual = Data.String("null")))
        .invalid
    )

    assertEq(obtained = codec.decode(Data.Object.Empty), expected = (none, none).valid)

    assertEq(
      obtained = codec.decode(
        Data.Object.of(
          "a" -> Data.String("foobar"),
          "c" -> Data.String("foobar")
        )
      ),
      expected = Violations
        .of(
          Step.Field("b") -> Violation(Constraint.Type("int"), actual = Data.String("null")),
          Step.Field("d") -> Violation(Constraint.Type("int"), actual = Data.String("null"))
        )
        .invalid
    )

  test("encode"):
    val codec = field("a", string) :* field("b", int)

    assertEq(
      obtained = codec.encode(("foobar", 42)),
      expected = Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42))
    )

  test("encode: nullable"):
    val codec = field("a", string.nullable).toRecord

    assertEq(
      obtained = codec.encode("foobar".some),
      expected = Data.Object.of("a" -> Data.String("foobar"))
    )

    assertEq(obtained = codec.encode(none), expected = Data.Object.Empty)

  test("encode: optional"):
    val codec = (field("a", string) :* field("b", int)).optional

    assertEq(
      obtained = codec.encode(("foobar", 42).some),
      expected = Data.Object.of("a" -> Data.String("foobar"), "b" -> Data.Number(42))
    )

    assertEq(obtained = codec.encode(none), expected = Data.Object.Empty)
