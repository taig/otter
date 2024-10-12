package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import munit.FunSuite

final class SumMergedTest extends FunSuite:
  test("encode"):
    val codec = sum.merged {
      branch("foo", record(field("a", string) :* field("b", int))) :+
        branch("bar", record(field("c", string)))
    }

    assertEquals(
      obtained = codec.encode(("foobar", 42).asLeft),
      expected = Data.Object.of(
        Discriminator.Merged.Default.identifier -> Data.String("foo"),
        "a" -> Data.String("foobar"),
        "b" -> Data.Number(42)
      )
    )

    assertEquals(
      obtained = codec.encode("foobar".asRight),
      expected = Data.Object.of(
        Discriminator.Merged.Default.identifier -> Data.String("bar"),
        "c" -> Data.String("foobar")
      )
    )

  test("encode: discriminator"):
    val discriminator: Discriminator.Merged = Discriminator.Merged(identifier = "x")
    val codec = sum
      .merged(
        branch("foo", record(field("a", string) :* field("b", int))) :+
          branch("bar", record(field("c", string)))
      )
      .discriminator(discriminator)

    assertEquals(
      obtained = codec.encode(("foobar", 42).asLeft),
      expected = Data.Object.of(
        discriminator.identifier -> Data.String("foo"),
        "a" -> Data.String("foobar"),
        "b" -> Data.Number(42)
      )
    )

  test("encode: discriminator (conflict)"):
    val discriminator: Discriminator.Merged = Discriminator.Merged(identifier = "x")
    val codec = sum
      .merged(
        branch("foo", record(field(discriminator.identifier, string) :* field("b", int))) :+
          branch("bar", record(field("c", string)))
      )
      .discriminator(discriminator)

    assertEquals(
      obtained = codec.encode(("foobar", 42).asLeft),
      expected = Data.Object.of(
        discriminator.identifier -> Data.String("foo"),
        "b" -> Data.Number(42)
      )
    )
