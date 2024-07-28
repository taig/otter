package io.taig.otter

import cats.syntax.all.*
import munit.FunSuite
import munit.Location
import munit.Compare
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Dsl.*

final class TupleTest extends FunSuite:
  test("encode"):
    assertEquals(
      obtained = tuple(field("foo", string) :* field("bar", int)).encode(("foobar", 42)),
      expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
    )
