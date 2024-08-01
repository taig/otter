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
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int)).discriminator(discriminator)

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.Object.of(
        discriminator.identifier -> Data.String("foo"),
        discriminator.value -> Data.String("foobar")
      )
    )

  test("encode: discriminator (conflict)"):
    val discriminator: Discriminator.Nested = Discriminator.Nested(identifier = "x", value = "x")
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int)).discriminator(discriminator)

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

  test("decode: identifier unknown"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

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

  test("decode: identifier missing"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(Discriminator.Nested.Default.value -> Data.String("foobar"))
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.identifier),
          Violation(Constraint.Type("string"), actual = Data.String("null"))
        )
        .invalid
    )

  test("decode: identifier invalid"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.Number(42),
          Discriminator.Nested.Default.value -> Data.String("foobar")
        )
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.identifier),
          Violation(Constraint.Type("string"), actual = Data.String("number"))
        )
        .invalid
    )

  test("decode: value missing"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(Discriminator.Nested.Default.identifier -> Data.String("foo"))
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.value),
          Violation(Constraint.Type("string"), actual = Data.String("null"))
        )
        .invalid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(Discriminator.Nested.Default.identifier -> Data.String("bar"))
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.value),
          Violation(Constraint.Type("number"), actual = Data.String("null"))
        )
        .invalid
    )

  test("decode: value invalid"):
    val codec = sum.nested(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.String("foo"),
          Discriminator.Nested.Default.value -> Data.Array.Empty
        )
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.value),
          Violation(Constraint.Type("string"), actual = Data.String("array"))
        )
        .invalid
    )

    assertEquals(
      obtained = codec.decode(
        Data.Object.of(
          Discriminator.Nested.Default.identifier -> Data.String("bar"),
          Discriminator.Nested.Default.value -> Data.Array.Empty
        )
      ),
      expected = Violations
        .namespaceNec(
          History.Step.Field(Discriminator.Nested.Default.value),
          Violation(Constraint.Type("number"), actual = Data.String("array"))
        )
        .invalid
    )
