package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.component.JsonComponent.*
import zio.*
import zio.Scope
import zio.test.*
import zio.test.Spec
import zio.test.TestEnvironment
import zio.test.ZIOSpecDefault

object JsonCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceEncoderTest")(
    test("Json.Constant"):
      val result = JsonCirceEncoder.encode(constant(string, "foobar"), ())
      assertTrue(result == CirceJson.fromString("foobar"))
    ,
    test("Json.Collection"):
      val result = JsonCirceEncoder.encode(collection.list(string), List("foo", "bar", "baz"))
      assertTrue(result == CirceJson.arr("foo".asJson, "bar".asJson, "baz".asJson))
    ,
    test("Json.Dictionary"):
      val result =
        JsonCirceEncoder.encode(dictionary.list(string), List("foo" -> "foo", "bar" -> "bar", "baz" -> "baz"))
      assertTrue(result == CirceJson.obj("foo" := "foo".asJson, "bar" := "bar".asJson, "baz" := "baz".asJson))
    ,
    test("Json.Record"):
      val result = JsonCirceEncoder.encode(
        field("foo", string) :* field("bar", int) :* field("baz", boolean),
        ("John Doe", 42, true)
      )
      val expected = CirceJson.obj("foo" := "John Doe", "bar" := 42, "baz" := true)

      assertTrue(result == expected)
    ,
    test("Json.Record: optional"):
      val schema = field("foo", string) :* field("bar", int).optional

      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42.some)) == CirceJson.obj("foo" := "John Doe", "bar" := 42),
        JsonCirceEncoder.encode(schema, ("John Doe", none)) == CirceJson.obj("foo" := "John Doe")
      )
    ,
    test("Json.Record: RNil"):
      val result = JsonCirceEncoder.encode(RNil, ())

      assertTrue(result == CirceJson.obj())
    ,
    test("Json.Tuple"):
      val result = JsonCirceEncoder.encode(string :* int :* boolean, ("John Doe", 42, true))
      val expected = CirceJson.arr("John Doe".asJson, 42.asJson, true.asJson)

      assertTrue(result == expected)
    ,
    test("Json.Tuple: TNil"):
      val result = JsonCirceEncoder.encode(TNil, ())

      assertTrue(result == CirceJson.arr())
  )
