package io.taig.otter.codec

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment
import zio.*
import zio.test.*
import io.taig.otter.component.JsonComponent.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import cats.syntax.all.*

object JsonCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceEncoderTest")(
    test("Constant"):
      val result = JsonCirceEncoder.encode(constant(string, "foobar"), "foo")
      assertTrue(result == CirceJson.fromString("foobar"))
    ,
    test("Collection"):
      val result = JsonCirceEncoder.encode(collection.list(string), List("foo", "bar", "baz"))
      assertTrue(result == CirceJson.arr("foo".asJson, "bar".asJson, "baz".asJson))
    ,
    test("Dictionary"):
      val result =
        JsonCirceEncoder.encode(dictionary.list(string), List("foo" -> "foo", "bar" -> "bar", "baz" -> "baz"))
      assertTrue(result == CirceJson.obj("foo" := "foo".asJson, "bar" := "bar".asJson, "baz" := "baz".asJson))
    ,
    test("Record"):
      val result = JsonCirceEncoder.encode(
        field("foo", string) :* field("bar", int) :* field("baz", boolean),
        ("John Doe", 42, true)
      )
      val expected = CirceJson.obj("foo" := "John Doe", "bar" := 42, "baz" := true)

      assertTrue(result == expected)
    ,
    test("Record: optional"):
      val schema = field("foo", string) :* field("bar", int).optional

      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42.some)) == CirceJson.obj("foo" := "John Doe", "bar" := 42),
        JsonCirceEncoder.encode(schema, ("John Doe", none)) == CirceJson.obj("foo" := "John Doe")
      )
    ,
    test("Record: RNil"):
      val result = JsonCirceEncoder.encode(RNil, ())

      assertTrue(result == CirceJson.obj())
    ,
    test("Tuple"):
      val result = JsonCirceEncoder.encode(string :* int :* boolean, ("John Doe", 42, true))
      val expected = CirceJson.arr("John Doe".asJson, 42.asJson, true.asJson)

      assertTrue(result == expected)
    ,
    test("Tuple: TNil"):
      val result = JsonCirceEncoder.encode(TNil, ())

      assertTrue(result == CirceJson.arr())
  )
