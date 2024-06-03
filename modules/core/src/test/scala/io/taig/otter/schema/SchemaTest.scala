package io.taig.otter.schema

import munit.FunSuite
import io.taig.otter.Decoder
import io.taig.otter.Encoder
import io.taig.otter.Schemas
import munit.Location
import munit.Compare
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

abstract class SchemaTest[A] extends FunSuite:
  schemas: Schemas =>

  def decoder: Decoder[Schema.Reader, A]

  def encoder: Encoder[Schema.Writer, A]

  def test[B](schema: Schema[B], value: B)(using Location): Unit = assertEquals(
    obtained = decoder(schema, encoder(schema, value)).valueOr(violations => fail(violations.toString)),
    expected = value
  )

  test("collection"):
    test(collection(string), Vector("foo", "bar", "baz"))
    test(collection(int), Vector(1, 2, 3))

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
