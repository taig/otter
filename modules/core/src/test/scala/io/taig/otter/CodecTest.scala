package io.taig.otter

import cats.syntax.all.*
import munit.FunSuite
import munit.Location
import munit.Compare
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Dsl.*

final class CodecTest[A] extends FunSuite:
  def test[B](codec: Codec[B], value: B)(using Location): Unit = assertEquals(
    obtained = codec.decode(codec.encode(value)).valueOr(violations => fail(violations.toString)),
    expected = value
  )

  test("collection: vector"):
    test(collection.vector(string), Vector("foo", "bar", "baz"))
    test(collection.vector(int), Vector(1, 2, 3))
    test(collection.vector(int.optional), Vector(1.some, none, 3.some))

  test("collection: optional"):
    test(collection.vector(int).optional, Vector(1, 2, 3).some)
    test(collection.vector(int).optional, none)

  test("collection: list"):
    test(collection.list(string), List("foo", "bar", "baz"))
    test(collection.list(string.optional), List("foo".some, none, "baz".some))

  test("record"):
    test(field("foo", int) :* field("bar", string) :* field("baz", long), (42, "foobar", 0L))

  test("primitive: bigDecimal"):
    test(bigDecimal, JBigDecimal.valueOf(Double.MinValue))
    test(bigDecimal, JBigDecimal.valueOf(Double.MaxValue))
    test(bigDecimal, JBigDecimal.valueOf(0d))

  test("primitive: bigInteger"):
    test(bigInteger, JBigInteger.valueOf(Long.MinValue))
    test(bigInteger, JBigInteger.valueOf(Long.MaxValue))
    test(bigInteger, JBigInteger.valueOf(0L))

  test("primitive: boolean"):
    test(boolean, true)
    test(boolean, false)

  test("primitive: double"):
    test(double, Double.MinValue)
    test(double, Double.MaxValue)
    test(double, 0d)

  test("primitive: int"):
    test(int, Int.MinValue)
    test(int, Int.MaxValue)
    test(int, 0)

  test("primitive: long"):
    test(long, Long.MinValue)
    test(long, Long.MaxValue)
    test(long, 0)

  test("primitive: string"):
    test(string, "")
    test(string, "foobar")
    test(string, "öäüß@§&%")

  test("primitive: optional"):
    test(string.optional, "foobar".some)
    test(int.optional, 0.some)
    test(string.optional, "".some)
    test(string.optional, none)
