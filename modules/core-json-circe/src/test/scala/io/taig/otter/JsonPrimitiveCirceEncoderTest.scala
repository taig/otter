package io.taig.otter

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment
import io.taig.otter.codec.JsonPrimitiveCirceEncoder
import io.taig.otter.component.JsonComponent.*
import zio.*
import zio.test.*
import io.circe.Json as CirceJson

object JsonPrimitiveCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonPrimitiveCirceEncoderTest")(
    test("Primitive.Boolean: true"):
      val result = JsonPrimitiveCirceEncoder.encode(boolean, true)
      assertTrue(result == CirceJson.fromBoolean(true))
    ,
    test("Primitive.Coerce.Boolean: true"):
      val result = JsonPrimitiveCirceEncoder.encode(coerce(boolean), true)
      assertTrue(result == CirceJson.fromBoolean(true))
    ,
    test("Primitive.Coerce.Boolean: false"):
      val result = JsonPrimitiveCirceEncoder.encode(coerce(boolean), false)
      assertTrue(result == CirceJson.fromBoolean(false))
    ,
    test("Primitive.Coerce.Number: Int"):
      val result = JsonPrimitiveCirceEncoder.encode(coerce(int), 42)
      assertTrue(result == CirceJson.fromInt(42))
    ,
    test("Primitive.Coerce.Text: Boolean"):
      val result = JsonPrimitiveCirceEncoder.encode(coerce(string), "true")
      assertTrue(result == CirceJson.fromString("true"))
    ,
    test("Primitive.Coerce.Text: String"):
      val result = JsonPrimitiveCirceEncoder.encode(coerce(string), "foobar")
      assertTrue(result == CirceJson.fromString("foobar"))
    ,
    test("Primitive.Boolean: false"):
      val result = JsonPrimitiveCirceEncoder.encode(boolean, false)
      assertTrue(result == CirceJson.fromBoolean(false))
    ,
    test("Primitive.Number: BigDecimal"):
      val result = JsonPrimitiveCirceEncoder.encode(bigDecimal, BigDecimal(42))
      assertTrue(result == CirceJson.fromBigDecimal(BigDecimal(42)))
    ,
    test("Primitive.Number: BitInt"):
      val result = JsonPrimitiveCirceEncoder.encode(bigInt, BigInt(42))
      assertTrue(result == CirceJson.fromBigInt(BigInt(42)))
    ,
    test("Primitive.Number: Double"):
      val result = JsonPrimitiveCirceEncoder.encode(double, 42.0)
      assertTrue(result == CirceJson.fromDoubleOrNull(42.0))
    ,
    test("Primitive.Number: Float"):
      val result = JsonPrimitiveCirceEncoder.encode(float, 42.0f)
      assertTrue(result == CirceJson.fromFloatOrNull(42.0f))
    ,
    test("Primitive.Number: Int"):
      val result = JsonPrimitiveCirceEncoder.encode(int, 42)
      assertTrue(result == CirceJson.fromInt(42))
    ,
    test("Primitive.Number: Long"):
      val result = JsonPrimitiveCirceEncoder.encode(long, 42L)
      assertTrue(result == CirceJson.fromLong(42L))
    ,
    test("Primitive.Text"):
      val result = JsonPrimitiveCirceEncoder.encode(string, "foobar")
      assertTrue(result == CirceJson.fromString("foobar"))
  )
