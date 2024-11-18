package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*

final class CollectionTest extends OtterSuite:
  test("encode"):
    val codec = collection.vector(string)

    assertEq(
      obtained = codec.encode(Vector("foo", "bar", "baz")),
      expected = Data.Array.of(Data.String("foo"), Data.String("bar"), Data.String("baz"))
    )

    assertEq(obtained = codec.encode(Vector.empty), expected = Data.Array.Empty)

  test("decode"):
    val codec = collection.vector(string)

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.String("foo"), Data.String("bar"), Data.String("baz"))),
      expected = Vector("foo", "bar", "baz").valid
    )

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.String("foo"), Data.Array.Empty, Data.Object.Empty)),
      expected = Violations
        .of(
          Step.Index(1) -> Violation(Constraint.Type("string"), actual = Data.String("array")),
          Step.Index(2) -> Violation(Constraint.Type("string"), actual = Data.String("object"))
        )
        .invalid
    )

    assertEq(obtained = codec.decode(Data.Array.Empty), expected = Vector.empty.valid)

  test("decode: nullable"):
    val codec = collection.vector(string).nullable

    assertEq(
      obtained = codec.decode(Data.Array.of(Data.String("foo"))),
      expected = Vector("foo").some.valid
    )

    assertEq(obtained = codec.decode(Data.Null), expected = none.valid)
