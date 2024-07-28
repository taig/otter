package io.taig.otter

import cats.syntax.all.*
import munit.FunSuite
import munit.Location
import munit.Compare
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Dsl.*

final class TupleTest extends FunSuite:
  test("decode"):
    tuple {
      field("foo", string)
    }
    val x: Tuple[String] = ???
    ???
