package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import io.taig.validation.Violation
import zio.Scope
import zio.test.*

object JsonCirceDecoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceDecoderTest")(
    test("Json.Primitive"):
      assertTrue(
        JsonCirceDecoder.decode(string, CirceJson.fromString("foobar")) == "foobar".valid,
        JsonCirceDecoder.decode(int, CirceJson.fromInt(42)) == 42.valid,
        JsonCirceDecoder.decode(boolean, CirceJson.fromBoolean(true)) == true.valid
      )
    ,
    test("Json.Primitive: type mismatch"):
      val result = JsonCirceDecoder.decode(int, CirceJson.fromString("foobar"))
      val expected = Violations(
        Violation(constraint = Constraint.Generic.Type(name = "int"), actual = "string", hint = none)
      ).invalid

      assertTrue(result == expected)
    ,
    test("Json.Coerce: accepts the laxer representation"):
      assertTrue(
        JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromString("true")) == true.valid,
        JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromBoolean(false)) == false.valid,
        JsonCirceDecoder.decode(coerce(int), CirceJson.fromString("42")) == 42.valid,
        JsonCirceDecoder.decode(coerce(int), CirceJson.fromInt(42)) == 42.valid,
        JsonCirceDecoder.decode(coerce(string), CirceJson.fromInt(42)) == "42".valid
      )
    ,
    test("Json.Coerce: still rejects what it cannot read"):
      val result = JsonCirceDecoder.decode(coerce(boolean), CirceJson.fromInt(42))
      assertTrue(result.isInvalid)
    ,
    test("Json.Collection"):
      val value = CirceJson.arr("foo".asJson, "bar".asJson)
      assertTrue(JsonCirceDecoder.decode(collection.list(string), value) == List("foo", "bar").valid)
    ,
    test("Json.Dictionary"):
      val value = CirceJson.obj("foo" := "1", "bar" := "2")
      assertTrue(JsonCirceDecoder.decode(dictionary.list(string), value) == List("foo" -> "1", "bar" -> "2").valid)
    ,
    test("Json.Record"):
      val schema = field("foo", string) :* field("bar", int) :* field("baz", boolean)
      val value = CirceJson.obj("foo" := "John Doe", "bar" := 42, "baz" := true)
      assertTrue(JsonCirceDecoder.decode(schema, value) == ("John Doe", 42, true).valid)
    ,
    test("Json.Record: optional field"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj("foo" := "x", "bar" := 42)) == ("x", 42.some).valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("foo" := "x")) == ("x", none).valid
      )
    ,
    test("Json.Record: an optional field accepts an explicit null"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj("foo" := "x", "bar" := CirceJson.Null)) ==
          ("x", none).valid
      )
    ,
    test("Json.Record: a defaulted field accepts an explicit null"):
      val schema = field("bar", int).optional(7).toRecord
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := CirceJson.Null)) == 7.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj()) == 7.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := 1)) == 1.valid
      )
    ,
    test("Json.Record: a required field still rejects null"):
      val schema = field("bar", int).toRecord
      assertTrue(JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := CirceJson.Null)).isInvalid)
    ,
    test("Json.Record: a field holding an optional schema wants its key"):
      val schema = field("bar", int.optional).toRecord
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := CirceJson.Null)) == none.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := 1)) == 1.some.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj()).isInvalid
      )
    ,
    test("Json.Record: a strict omitted field rejects a null"):
      val schema = field("bar", int).optional.omitted.strict.toRecord
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj()) == none.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := 1)) == 1.some.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := CirceJson.Null)).isInvalid
      )
    ,
    test("Json.Record: a strict nullable field rejects a missing key"):
      val schema = field("bar", int).optional.nullable.strict.toRecord
      assertTrue(
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := CirceJson.Null)) == none.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj("bar" := 1)) == 1.some.valid,
        JsonCirceDecoder.decode(schema, CirceJson.obj()).isInvalid
      )
    ,
    test("Json.Record: leniency is what reading does anyway"):
      val nullable = field("bar", int).optional.nullable.toRecord
      val omitted = field("bar", int).optional.omitted.toRecord
      val value = CirceJson.obj("bar" := CirceJson.Null)
      assertTrue(JsonCirceDecoder.decode(nullable, value) == JsonCirceDecoder.decode(omitted, value))
    ,
    test("Json.Record: only a strict field tells two layers of absence apart"):
      val strict = field("bar", int.optional).optional.omitted.strict.toRecord
      val lenient = field("bar", int.optional).optional.toRecord
      val value = CirceJson.obj("bar" := CirceJson.Null)
      assertTrue(
        JsonCirceDecoder.decode(strict, CirceJson.obj()) == none.valid,
        JsonCirceDecoder.decode(strict, value) == none.some.valid,
        JsonCirceDecoder.decode(strict, CirceJson.obj("bar" := 1)) == 1.some.some.valid,
        JsonCirceDecoder.decode(lenient, value) == none.valid
      )
    ,
    test("Json.Record: missing field"):
      val schema = field("foo", string) :* field("bar", int)
      val result = JsonCirceDecoder.decode(schema, CirceJson.obj("foo" := "x"))
      assertTrue(result.isInvalid)
    ,
    test("Json.Tuple"):
      val schema = TNil :* string :* int :* boolean
      val value = CirceJson.arr("John Doe".asJson, 42.asJson, true.asJson)
      assertTrue(JsonCirceDecoder.decode(schema, value) == ("John Doe", 42, true).valid)
    ,
    test("Json.Tuple: wrong arity"):
      val schema = TNil :* string :* int
      assertTrue(JsonCirceDecoder.decode(schema, CirceJson.arr("x".asJson)).isInvalid)
    ,
    test("Json.Constant"):
      assertTrue(
        JsonCirceDecoder.decode(constant(string, "foobar"), CirceJson.fromString("foobar")) == ().valid,
        JsonCirceDecoder.decode(constant(string, "foobar"), CirceJson.fromString("barfoo")).isInvalid
      )
    ,
    test("Json.Record: case class"):
      val value = CirceJson.obj("title" := "Dune", "pages" := 412, "read" := true)
      assertTrue(JsonCirceDecoder.decode(json.book, value) == Book("Dune", 412, true).valid)
    ,
    test("Json.Union: enum"):
      assertTrue(
        JsonCirceDecoder.decode(json.shape, CirceJson.obj("radius" := 1.5)) == Shape.Circle(1.5).valid,
        JsonCirceDecoder.decode(json.shape, CirceJson.obj("side" := 2.0)) == Shape.Square(2.0).valid,
        JsonCirceDecoder.decode(json.shape, CirceJson.obj("base" := 3.0, "height" := 4.0)) ==
          Shape.Triangle(3.0, 4.0).valid
      )
    ,
    test("violations carry the path to the failure"):
      val schema = field("foo", string) :* field("bar", collection.list(int))
      val value = CirceJson.obj("foo" := "x", "bar" := CirceJson.arr(1.asJson, "nope".asJson))

      val steps = JsonCirceDecoder.decode(schema, value) match
        case Validated.Invalid(violations) => paths(violations)
        case Validated.Valid(_)            => Nil

      assertTrue(steps == List(List(Step.Field("bar"), Step.Index(1))))
    ,
    test("Json.Enumeration"):
      assertTrue(
        JsonCirceDecoder.decode(json.genre, CirceJson.fromString("history")) == Genre.History.valid,
        JsonCirceDecoder.decode(json.genre, CirceJson.fromString("nope")).isInvalid
      )
    ,
    test("read only schema"):
      assertTrue(JsonCirceDecoder.decode(json.isbn, CirceJson.fromString("978")) == Isbn("978").valid)
  )

  private def paths(violations: Violations): List[List[Step]] = violations match
    case Violations.Root(values, _) =>
      if values.isEmpty then List(Nil)
      else values.toList.flatMap((step, nested) => paths(nested).map(step :: _))
    case Violations.Namespace(values) =>
      values.toSortedMap.toList.flatMap((step, nested) => paths(nested).map(step :: _))
