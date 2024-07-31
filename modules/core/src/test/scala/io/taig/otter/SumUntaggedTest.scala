package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*

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
