package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.JsonPrimitiveCirceDecoder
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.Violation
import zio.*
import zio.Scope
import zio.test.*
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault

object JsonPrimitiveCirceDecoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonPrimitiveCirceDecoderTest")(
    test("Json.Primitive.Boolean: true"):
      val result = JsonPrimitiveCirceDecoder.decode(boolean, CirceJson.fromBoolean(true))
      assertTrue(result == true.valid)
    ,
    test("Json.Primitive.Boolean: false"):
      val result = JsonPrimitiveCirceDecoder.decode(boolean, CirceJson.fromBoolean(false))
      assertTrue(result == false.valid)
    ,
    test("Json.Primitive.Boolean: invalid"):
      val result = JsonPrimitiveCirceDecoder.decode(boolean, CirceJson.fromString("foobar"))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "boolean"),
          actual = "string".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Primitive.Number: Int"):
      val result = JsonPrimitiveCirceDecoder.decode(int, CirceJson.fromInt(42))
      assertTrue(result == 42.valid)
    ,
    test("Json.Primitive.Number: Int (invalid)"):
      val result = JsonPrimitiveCirceDecoder.decode(int, CirceJson.fromFloatOrString(42.9f))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "int"),
          actual = "number".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Primitive.Number: Long"):
      val result = JsonPrimitiveCirceDecoder.decode(long, CirceJson.fromLong(42L))
      assertTrue(result == 42L.valid)
    ,
    test("Json.Primitive.Number: Long (invalid)"):
      val result = JsonPrimitiveCirceDecoder.decode(long, CirceJson.fromBoolean(false))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "long"),
          actual = "boolean".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Primitive.Text"):
      val result = JsonPrimitiveCirceDecoder.decode(string, CirceJson.fromString("foobar"))
      assertTrue(result == "foobar".valid)
    ,
    test("Json.Primitive.Text: invalid"):
      val result = JsonPrimitiveCirceDecoder.decode(string, CirceJson.fromInt(42))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "string"),
          actual = "number".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
  )
