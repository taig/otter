package io.taig.otter.codec

import zio.test.ZIOSpecDefault
import zio.Scope
import cats.syntax.all.*
import zio.test.Spec
import zio.test.TestEnvironment
import io.taig.otter.component.JsonComponent.*
import zio.*
import zio.test.*
import io.circe.Json as CirceJson
import io.taig.otter.codec.JsonPrimitiveCirceDecoder
import io.taig.otter.Violations
import io.taig.validation.Violation
import io.taig.otter.Constraint
import io.taig.data.syntax.*

object JsonPrimitiveCirceDecoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonPrimitiveCirceDecoderTest")(
    test("Primitive.Boolean: true"):
      val result = JsonPrimitiveCirceDecoder.decode(boolean, CirceJson.fromBoolean(true))
      assertTrue(result == true.valid)
    ,
    test("Primitive.Boolean: false"):
      val result = JsonPrimitiveCirceDecoder.decode(boolean, CirceJson.fromBoolean(false))
      assertTrue(result == false.valid)
    ,
    test("Primitive.Boolean: invalid"):
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
    test("Primitive.Coerce.Boolean: \"true\""):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(boolean), CirceJson.fromString("true"))
      assertTrue(result == true.valid)
    ,
    test("Primitive.Coerce.Boolean: false"):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(boolean), CirceJson.fromBoolean(false))
      assertTrue(result == false.valid)
    ,
    test("Primitive.Coerce.Boolean: invalid"):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(boolean), CirceJson.fromInt(42))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "boolean"),
          actual = "number".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Primitive.Coerce.Number: Int (\"42\")"):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(int), CirceJson.fromString("42"))
      assertTrue(result == 42.valid)
    ,
    test("Primitive.Coerce.Number: Int (42)"):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(int), CirceJson.fromInt(42))
      assertTrue(result == 42.valid)
    ,
    test("Primitive.Coerce.Number: Int (invalid)"):
      val result = JsonPrimitiveCirceDecoder.decode(coerce(int), CirceJson.fromString("foobar"))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "int"),
          actual = "string".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Primitive.Number: Int"):
      val result = JsonPrimitiveCirceDecoder.decode(int, CirceJson.fromInt(42))
      assertTrue(result == 42.valid)
    ,
    test("Primitive.Number: Int (invalid)"):
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
    test("Primitive.Number: Long"):
      val result = JsonPrimitiveCirceDecoder.decode(long, CirceJson.fromLong(42L))
      assertTrue(result == 42L.valid)
    ,
    test("Primitive.Number: Long (invalid)"):
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
    test("Primitive.Text"):
      val result = JsonPrimitiveCirceDecoder.decode(string, CirceJson.fromString("foobar"))
      assertTrue(result == "foobar".valid)
    ,
    test("Primitive.Text: invalid"):
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
