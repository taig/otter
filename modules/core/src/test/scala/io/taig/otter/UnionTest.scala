package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*

final class UnionTest extends OtterSuite:
  test("decode: untagged"):
    val codec = branch("a", string) :+ branch("b", int)

    assertEq(
      obtained = codec.decode(Data.String("foobar")),
      expected = "foobar".asLeft.valid
    )

    assertEq(
      obtained = codec.decode(Data.Number(42)),
      expected = 42.asRight.valid
    )

    assertEq(
      obtained = codec.decode(Data.Array.Empty),
      expected = Violations
        .of(
          Step.Field("a") -> Violation.tpe(name = "string", actual = "array"),
          Step.Field("b") -> Violation.tpe(name = "int", actual = "array")
        )
        .invalid
    )
