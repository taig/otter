package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.Violation
import zio.*
import zio.Scope
import zio.test.*
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault

object JsonCirceDecoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceDecoderTest")(
    test("Json.Coerce: boolean (\"true\")"):
      val result = JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromString("true"))
      assertTrue(result == true.valid)
    ,
    test("Json.Coerce: boolean (false)"):
      val result = JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromBoolean(false))
      assertTrue(result == false.valid)
    ,
    test("Json.Coerce: boolean (invalid)"):
      val result = JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromInt(42))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "boolean"),
          actual = "number".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Coerce: number (\"42\")"):
      val result = JsonCirceDecoder.decode(coerce(int), CirceJson.fromString("42"))
      assertTrue(result == 42.valid)
    ,
    test("Json.Coerce: number (42)"):
      val result = JsonCirceDecoder.decode(coerce(int), CirceJson.fromInt(42))
      assertTrue(result == 42.valid)
    ,
    test("Json.Coerce: number (invalid)"):
      val result = JsonCirceDecoder.decode(coerce(int), CirceJson.fromString("foobar"))
      val expected = Violations(
        violation = Violation(
          constraint = Constraint.Generic.Type(name = "int"),
          actual = "string".asData,
          hint = none
        )
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Coerce: string"):
      val result = JsonCirceDecoder.decode(coerce(string), CirceJson.fromString("foobar"))
      assertTrue(result == "foobar".valid)
  )
