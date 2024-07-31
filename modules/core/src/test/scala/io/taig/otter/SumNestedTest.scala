package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*

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
