package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Absence
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object JsonCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceEncoderTest")(
    test("Json.Primitive"):
      assertTrue(
        JsonCirceEncoder.encode(string, "foobar") == CirceJson.fromString("foobar"),
        JsonCirceEncoder.encode(int, 42) == CirceJson.fromInt(42),
        JsonCirceEncoder.encode(boolean, true) == CirceJson.fromBoolean(true)
      )
    ,
    test("Json.Constant"):
      assertTrue(JsonCirceEncoder.encode(constant(string, "foobar"), ()) == CirceJson.fromString("foobar"))
    ,
    test("Json.Coerce"):
      assertTrue(JsonCirceEncoder.encode(coerce(boolean), true) == CirceJson.fromBoolean(true))
    ,
    test("Json.Collection"):
      assertTrue(
        JsonCirceEncoder.encode(collection.list(string), List("foo", "bar")) ==
          CirceJson.arr("foo".asJson, "bar".asJson)
      )
    ,
    test("Json.Dictionary"):
      assertTrue(
        JsonCirceEncoder.encode(dictionary.list(string), List("foo" -> "1", "bar" -> "2")) ==
          CirceJson.obj("foo" := "1", "bar" := "2")
      )
    ,
    test("Json.Record"):
      val schema = field("foo", string) :* field("bar", int) :* field("baz", boolean)
      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42, true)) ==
          CirceJson.obj("foo" := "John Doe", "bar" := 42, "baz" := true)
      )
    ,
    test("Json.Record: optional field"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42.some)) == CirceJson.obj("foo" := "John Doe", "bar" := 42),
        JsonCirceEncoder.encode(schema, ("John Doe", none)) == CirceJson.obj("foo" := "John Doe")
      )
    ,
    test("Json.Record: nullable optional field"):
      val schema = field("foo", string) :* field("bar", int).optional.nullable
      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42.some)) == CirceJson.obj("foo" := "John Doe", "bar" := 42),
        JsonCirceEncoder.encode(schema, ("John Doe", none)) ==
          CirceJson.obj("foo" := "John Doe", "bar" := CirceJson.Null)
      )
    ,
    test("Json.Record: omitting is what a field does anyway"):
      val implicitly = field("foo", string) :* field("bar", int).optional
      val explicitly = field("foo", string) :* field("bar", int).optional.omitted
      assertTrue(
        JsonCirceEncoder.encode(implicitly, ("John Doe", none)) ==
          JsonCirceEncoder.encode(explicitly, ("John Doe", none))
      )
    ,
    test("Json.Record: the attribute survives .optional"):
      val before = field("bar", int).nullable.optional.toRecord
      val after = field("bar", int).optional.nullable.toRecord
      assertTrue(
        JsonCirceEncoder.encode(before, none) == CirceJson.obj("bar" := CirceJson.Null),
        JsonCirceEncoder.encode(after, none) == CirceJson.obj("bar" := CirceJson.Null)
      )
    ,
    test("Json.Record: a globally set attribute is read"):
      val schema = field("bar", int).optional.attr(Keys.absence, Absence.Empty).toRecord
      assertTrue(JsonCirceEncoder.encode(schema, none) == CirceJson.obj("bar" := CirceJson.Null))
    ,
    test("Json.Record: the json namespace wins over the global one"):
      val schema = field("bar", int).optional
        .attr(Keys.absence, Absence.Empty)
        .attr(Json.Namespace, Keys.absence, Absence.Omit)
        .toRecord
      assertTrue(JsonCirceEncoder.encode(schema, none) == CirceJson.obj())
    ,
    test("Json.Record: a defaulted field writes whatever it holds"):
      val schema = field("bar", int).optional(0).nullable.toRecord
      assertTrue(
        JsonCirceEncoder.encode(schema, 42) == CirceJson.obj("bar" := 42),
        JsonCirceEncoder.encode(schema, 0) == CirceJson.obj("bar" := 0)
      )
    ,
    test("Json.Record: RNil"):
      assertTrue(JsonCirceEncoder.encode(RNil, ()) == CirceJson.obj())
    ,
    test("Json.Tuple"):
      val schema = TNil :* string :* int :* boolean
      assertTrue(
        JsonCirceEncoder.encode(schema, ("John Doe", 42, true)) ==
          CirceJson.arr("John Doe".asJson, 42.asJson, true.asJson)
      )
    ,
    test("Json.Tuple: TNil"):
      assertTrue(JsonCirceEncoder.encode(TNil, ()) == CirceJson.arr())
    ,
    test("Json.Record: case class"):
      assertTrue(
        JsonCirceEncoder.encode(json.book, Book("Dune", 412, true)) ==
          CirceJson.obj("title" := "Dune", "pages" := 412, "read" := true)
      )
    ,
    test("Json.Union: enum"):
      assertTrue(
        JsonCirceEncoder.encode(json.shape, Shape.Circle(1.5)) == CirceJson.obj("radius" := 1.5),
        JsonCirceEncoder.encode(json.shape, Shape.Square(2.0)) == CirceJson.obj("side" := 2.0),
        JsonCirceEncoder.encode(json.shape, Shape.Triangle(3.0, 4.0)) ==
          CirceJson.obj("base" := 3.0, "height" := 4.0)
      )
    ,
    test("recursive schema"):
      val tree = Tree(1, List(Tree(2, Nil), Tree(3, Nil)))
      assertTrue(
        JsonCirceEncoder.encode(json.tree, tree) == CirceJson.obj(
          "value" := 1,
          "children" := CirceJson.arr(
            CirceJson.obj("value" := 2, "children" := CirceJson.arr()),
            CirceJson.obj("value" := 3, "children" := CirceJson.arr())
          )
        )
      )
    ,
    test("Json.Enumeration"):
      assertTrue(
        JsonCirceEncoder.encode(json.genre, Genre.Fiction) == CirceJson.fromString("fiction"),
        JsonCirceEncoder.encode(json.genre, Genre.Poetry) == CirceJson.fromString("poetry")
      )
    ,
    test("write only schema"):
      assertTrue(JsonCirceEncoder.encode(json.title, Book("Dune", 412, true)) == CirceJson.fromString("Dune"))
  )
