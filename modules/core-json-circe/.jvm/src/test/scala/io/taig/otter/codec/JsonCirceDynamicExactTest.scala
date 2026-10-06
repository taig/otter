package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.Data
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object JsonCirceDynamicExactTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceDynamicExactTest")(
    test("the JVM circe model retains large integers and long decimals"):
      val bigInteger = new java.math.BigInteger("92233720368547758081234567890")
      val decimal = new java.math.BigDecimal("0.12345678901234567890123456789012")

      assertTrue(
        JsonCirceInterpreter.decode(dynamic.any, bigInteger.toString) == Validated.valid(bigInteger: Data),
        JsonCirceInterpreter.decode(dynamic.any, decimal.toString) == Validated.valid(decimal: Data),
        JsonCirceInterpreter.roundTrip(dynamic.any, bigInteger) == Validated.valid(bigInteger: Data),
        JsonCirceInterpreter.roundTrip(dynamic.any, decimal) == Validated.valid(decimal: Data)
      )
  )
