// package io.taig.otter

// import cats.syntax.all.*
// import io.taig.otter.Dsl.*
// import munit.FunSuite
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.taig.otter.validation.Step

// final class TupleTest extends FunSuite:
//   test("decode"):
//     val codec = tuple(field("foo", string) :* field("bar", int))

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42))),
//       expected = ("foobar", 42).valid
//     )

//     assertEquals(
//       obtained = codec.decode(Data.Null),
//       expected = Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))).invalid
//     )

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.Array.Empty, Data.String("foobar"))),
//       expected = Violations
//         .of(
//           Step.Field("foo") -> Violation(Constraint.Type("string"), actual = Data.String("array")),
//           Step.Field("bar") -> Violation(Constraint.Type("number"), actual = Data.String("string"))
//         )
//         .invalid
//     )

//   test("decode: optional"):
//     val codec = tuple(field("foo", string) :* field("bar", int)).optional

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42))),
//       expected = ("foobar", 42).some.valid
//     )

//     assertEquals(obtained = codec.decode(Data.Null), expected = none.valid)

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.Null, Data.Null)),
//       expected = none.valid
//     )

//   test("decode: optional (product)"):
//     val product =
//       tuple(field("a", string) :* field("b", int)).optional.zip(tuple(field("c", string) :* field("d", int)).optional)

//     assertEquals(
//       obtained =
//         product.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"), Data.Number(42))),
//       expected = (("foobar", 42).some, ("foobar", 42).some).valid
//     )

//     assertEquals(
//       obtained = product.decode(Data.Array.of(Data.Null, Data.Null, Data.String("foobar"), Data.Number(42))),
//       expected = (none, ("foobar", 42).some).valid
//     )

//     assertEquals(
//       obtained = product.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Null, Data.Null)),
//       expected = (("foobar", 42).some, none).valid
//     )

//     assertEquals(
//       obtained = product.decode(Data.Array.of(Data.Null, Data.Null, Data.Null, Data.Null)),
//       expected = (none, none).valid
//     )

//   test("decode: optional (nested)"):
//     val codec = tuple(field("foo", string.optional) :* field("bar", int.optional)).optional

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42))),
//       expected = ("foobar".some, 42.some).some.valid
//     )

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.Null, Data.Null)),
//       expected = none.valid
//     )

//     assertEquals(obtained = codec.decode(Data.Null), expected = none.valid)

//   test("decode: length"):
//     val codec = tuple(field("foo", string) :* field("bar", int))

//     assertEquals(
//       obtained = codec.decode(Data.Array.of()),
//       expected =
//         Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 2), actual = Data.Number(0))).invalid
//     )

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.String("foobar"))),
//       expected =
//         Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 2), actual = Data.Number(1))).invalid
//     )

//     assertEquals(
//       obtained = codec.decode(Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"))),
//       expected =
//         Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference = 2), actual = Data.Number(3))).invalid
//     )

//   test("encode"):
//     val codec = tuple(field("foo", string) :* field("bar", int))

//     assertEquals(
//       obtained = codec.encode(("foobar", 42)),
//       expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
//     )

//   test("encode: optional"):
//     val codec = tuple(field("foo", string) :* field("bar", int)).optional

//     assertEquals(
//       obtained = codec.encode(("foobar", 42).some),
//       expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
//     )

//     assertEquals(obtained = codec.encode(none), expected = Data.Null)

//   test("encode: optional (product)"):
//     val codec = tuple(field("a", string) :* field("b", int)).optional
//       .zip(tuple(field("c", string) :* field("d", int)).optional)

//     assertEquals(
//       obtained = codec.encode(("foobar", 42).some, ("foobar", 42).some),
//       expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"), Data.Number(42))
//     )

//     assertEquals(
//       obtained = codec.encode(none, ("foobar", 42).some),
//       expected = Data.Array.of(Data.Null, Data.Null, Data.String("foobar"), Data.Number(42))
//     )

//     assertEquals(
//       obtained = codec.encode(("foobar", 42).some, none),
//       expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Null, Data.Null)
//     )

//     assertEquals(
//       obtained = codec.encode(none, none),
//       expected = Data.Array.of(Data.Null, Data.Null, Data.Null, Data.Null)
//     )
