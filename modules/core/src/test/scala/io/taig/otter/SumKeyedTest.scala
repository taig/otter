package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*

final class SumKeyedTest extends FunSuite:
  test("encode"):
    val codec = sum.keyed(branch("foo", string) :+ branch("bar", int))

    assertEquals(
      obtained = codec.encode("foobar".asLeft),
      expected = Data.Object.of("foo" -> Data.String("foobar"))
    )

    assertEquals(
      obtained = codec.encode(42.asRight),
      expected = Data.Object.of("bar" -> Data.Number(42))
    )
