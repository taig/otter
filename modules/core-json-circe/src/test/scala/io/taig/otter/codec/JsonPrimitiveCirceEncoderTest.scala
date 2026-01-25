package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.codec.JsonPrimitiveCirceEncoder
import io.taig.otter.component.JsonComponent.*
import zio.*
import zio.Scope
import zio.test.*
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault

object JsonPrimitiveCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonPrimitiveCirceEncoderTest")(
    test("Json.Primitive.Boolean: true"):
      val result = JsonPrimitiveCirceEncoder.encode(boolean, true)
      assertTrue(result == CirceJson.fromBoolean(true))
    ,
    test("Json.Primitive.Boolean: false"):
      val result = JsonPrimitiveCirceEncoder.encode(boolean, false)
      assertTrue(result == CirceJson.fromBoolean(false))
    ,
    test("Json.Primitive.Number: BigDecimal"):
      val result = JsonPrimitiveCirceEncoder.encode(bigDecimal, BigDecimal(42))
      assertTrue(result == CirceJson.fromBigDecimal(BigDecimal(42)))
    ,
    test("Json.Primitive.Number: BigInt"):
      val result = JsonPrimitiveCirceEncoder.encode(bigInt, BigInt(42))
      assertTrue(result == CirceJson.fromBigInt(BigInt(42)))
    ,
    test("Json.Primitive.Number: Double"):
      val result = JsonPrimitiveCirceEncoder.encode(double, 42.0)
      assertTrue(result == CirceJson.fromDoubleOrNull(42.0))
    ,
    test("Json.Primitive.Number: Float"):
      val result = JsonPrimitiveCirceEncoder.encode(float, 42.0f)
      assertTrue(result == CirceJson.fromFloatOrNull(42.0f))
    ,
    test("Json.Primitive.Number: Int"):
      val result = JsonPrimitiveCirceEncoder.encode(int, 42)
      assertTrue(result == CirceJson.fromInt(42))
    ,
    test("Json.Primitive.Number: Long"):
      val result = JsonPrimitiveCirceEncoder.encode(long, 42L)
      assertTrue(result == CirceJson.fromLong(42L))
    ,
    test("Json.Primitive.Text"):
      val result = JsonPrimitiveCirceEncoder.encode(string, "foobar")
      assertTrue(result == CirceJson.fromString("foobar"))
  )
