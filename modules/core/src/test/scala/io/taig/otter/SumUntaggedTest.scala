package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

final class SumUntaggedTest extends FunSuite:
  test("encode"):
    val codec = sum.untagged(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.String("foobar")
    )

    assertEquals(
      obtained = codec.encode(42.asRight),
      expected = Data.Number(42)
    )

  test("decode"):
    val codec = sum.untagged(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(Data.String("foobar")),
      expected = "foobar".asLeft.valid
    )

    assertEquals(
      obtained = codec.decode(Data.Number(42)),
      expected = 42.asRight.valid
    )

    assertEquals(
      obtained = codec.decode(Data.Array.Empty),
      expected = Violations
        .of(
          Violations.namespaceNec(
            History.Step.Field("foo"),
            Violation(Constraint.Type("string"), actual = Data.String("array"))
          ),
          Violations.namespaceNec(
            History.Step.Field("bar"),
            Violation(Constraint.Type("number"), actual = Data.String("array"))
          )
        )
        .invalid
    )
