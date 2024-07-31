package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

final class SumNestedTest extends FunSuite:
  test("encode"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.Object.of(
        Discriminator.Nested.Default.identifier -> Data.String("foo"),
        Discriminator.Nested.Default.value -> Data.String("foobar")
      )
    )

    assertEquals(
      obtained = codec.encode(42.asRight),
      expected = Data.Object.of(
        Discriminator.Nested.Default.identifier -> Data.String("bar"),
        Discriminator.Nested.Default.value -> Data.Number(42)
      )
    )

  test("encode: discriminator"):
    val discriminator: Discriminator.Nested = Discriminator.Nested(identifier = "a", value = "b")
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int)).modifyDiscriminator(_ => discriminator)

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.Object.of(
        discriminator.identifier -> Data.String("foo"),
        discriminator.value -> Data.String("foobar")
      )
    )

  test("encode: discriminator (conflict)"):
    val discriminator: Discriminator.Nested = Discriminator.Nested(identifier = "x", value = "x")
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int)).modifyDiscriminator(_ => discriminator)

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.Object.of(discriminator.identifier -> Data.String("foo"))
    )

  test("decode"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.String("foo"),
          Discriminator.Nested.Default.value -> Data.String("foobar")
        )
      ),
      expected = "foobar".asLeft.valid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.String("bar"),
          Discriminator.Nested.Default.value -> Data.Number(42)
        )
      ),
      expected = 42.asRight.valid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.String("baz"),
          Discriminator.Nested.Default.value -> Data.String("foobar")
        )
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.identifier),
          Violation(Constraint.OneOf(List(Data.String("foo"), Data.String("bar"))), actual = Data.String("baz"))
        )
        .invalid
    )
