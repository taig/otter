package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*

final class TupleTest extends OtterSuite:
  test("decode"):
    val codec = string :* int :* boolean

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Boolean(true))),
      expected = ("foobar", 42, true).valid
    )

    assertEq(
      obtained = codec.decode(Data.Null),
      expected = Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))).invalid
    )

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.Array.Empty, Data.String("foobar"), Data.Number(0))),
      expected = Violations
        .of(
          Step.Index(0) -> Violation.tpe("string", actual = "array"),
          Step.Index(1) -> Violation.tpe("int", actual = "string"),
          Step.Index(2) -> Violation.tpe("boolean", actual = "number")
        )
        .invalid
    )

  test("decode: nullable"):
    val codec = (string :* int).nullable

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42))),
      expected = ("foobar", 42).some.valid
    )

    assertEq(obtained = codec.decode(Data.Null), expected = none.valid)

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.Null, Data.Null)),
      expected = Violations
        .of(
          Step.Index(0) -> Violation.tpe("string", actual = "null"),
          Step.Index(1) -> Violation.tpe("int", actual = "null")
        )
        .invalid
    )

  // test("decode: optional (product)"):
  //   val product =
  //     tuple(field("a", string) :* field("b", int)).nullable.zip(tuple(field("c", string) :* field("d", int)).nullable)

  //   assertEq(
  //     obtained =
  //       product.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"), Data.Number(42))),
  //     expected = (("foobar", 42).some, ("foobar", 42).some).valid
  //   )

  //   assertEq(
  //     obtained = product.decode(Data.Array.of(Data.Null, Data.Null, Data.String("foobar"), Data.Number(42))),
  //     expected = (none, ("foobar", 42).some).valid
  //   )

  //   assertEq(
  //     obtained = product.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Null, Data.Null)),
  //     expected = (("foobar", 42).some, none).valid
  //   )

  //   assertEq(
  //     obtained = product.decode(Data.Array.of(Data.Null, Data.Null, Data.Null, Data.Null)),
  //     expected = (none, none).valid
  //   )

  // test("decode: optional (nested)"):
  //   val codec = tuple(field("foo", string.nullable) :* field("bar", int.nullable)).nullable

  //   assertEq(
  //     obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42))),
  //     expected = ("foobar".some, 42.some).some.valid
  //   )

  //   assertEq(
  //     obtained = codec.decode(Data.Array.of(Data.Null, Data.Null)),
  //     expected = none.valid
  //   )

  //   assertEq(obtained = codec.decode(Data.Null), expected = none.valid)

  // test("decode: length"):
  //   val codec = tuple(field("foo", string) :* field("bar", int))

  //   assertEq(
  //     obtained = codec.decode(Data.Array.of()),
  //     expected =
  //       Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 2), actual = Data.Number(0))).invalid
  //   )

  //   assertEq(
  //     obtained = codec.decode(Data.Array.of(Data.String("foobar"))),
  //     expected =
  //       Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 2), actual = Data.Number(1))).invalid
  //   )

  //   assertEq(
  //     obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"))),
  //     expected =
  //       Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference = 2), actual = Data.Number(3))).invalid
  //   )

  // test("encode"):
  //   val codec = tuple(field("foo", string) :* field("bar", int))

  //   assertEq(
  //     obtained = codec.encode(("foobar", 42)),
  //     expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
  //   )

  // test("encode: optional"):
  //   val codec = tuple(field("foo", string) :* field("bar", int)).nullable

  //   assertEq(
  //     obtained = codec.encode(("foobar", 42).some),
  //     expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
  //   )

  //   assertEq(obtained = codec.encode(none), expected = Data.Null)

  // test("encode: optional (product)"):
  //   val codec = tuple(field("a", string) :* field("b", int)).nullable
  //     .zip(tuple(field("c", string) :* field("d", int)).nullable)

  //   assertEq(
  //     obtained = codec.encode(("foobar", 42).some, ("foobar", 42).some),
  //     expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"), Data.Number(42))
  //   )

  //   assertEq(
  //     obtained = codec.encode(none, ("foobar", 42).some),
  //     expected = Data.Array.of(Data.Null, Data.Null, Data.String("foobar"), Data.Number(42))
  //   )

  //   assertEq(
  //     obtained = codec.encode(("foobar", 42).some, none),
  //     expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Null, Data.Null)
  //   )

  //   assertEq(
  //     obtained = codec.encode(none, none),
  //     expected = Data.Array.of(Data.Null, Data.Null, Data.Null, Data.Null)
  //   )
