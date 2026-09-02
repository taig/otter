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

import java.util.UUID
import scala.collection.immutable.SortedMap

object JsonCirceEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceEncoderTest")(
    test("Json.Primitive"):
      assertTrue(
        JsonCirceEncoder.encode(string, "foobar") == CirceJson.fromString("foobar"),
        JsonCirceEncoder.encode(int, 42) == CirceJson.fromInt(42),
        JsonCirceEncoder.encode(boolean, true) == CirceJson.fromBoolean(true)
      )
    ,
    test("Json.Primitive: uuid"):
      val id = java.util.UUID.fromString("1c1a5f8e-6e33-4e34-8d2e-3f8b2f0e1a2b")
      assertTrue(JsonCirceEncoder.encode(uuid, id) == CirceJson.fromString(id.toString))
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
    test("Json.Dictionary: a map is written in key order"):
      assertTrue(
        JsonCirceEncoder.encode(dictionary.map(string), SortedMap("foo" -> "1", "bar" -> "2")) ==
          CirceJson.obj("bar" := "2", "foo" := "1")
      )
    ,
    test("Json.Dictionary: a typed key is printed by its schema"):
      val id = UUID.fromString("6b1a4a5c-3a1e-4f0e-9b7e-2f0f5b3c9a11")
      assertTrue(
        JsonCirceEncoder.encode(json.editions, SortedMap(id -> 3)) == CirceJson.obj(id.toString := 3)
      )
    ,
    test("Json.Dictionary: an integer key is written as the text it is"):
      assertTrue(
        JsonCirceEncoder.encode(json.printings, List(5 -> "first")) == CirceJson.obj("5" := "first")
      )
    ,
    test("Json.Branch: a name spelled as a value is the name it prints to"):
      val value = Shape.Circle(1.5)
      assertTrue(JsonCirceEncoder.encode(json.taggedShape, value) == JsonCirceEncoder.encode(json.shape, value))
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
    ,
    test("a member that only writes leaves the members after it where they are"):
      assertTrue(
        JsonCirceEncoder.encode(json.shelf, Shelf(Isbn("978"), 412, true)) ==
          CirceJson.obj("label" := "978", "pages" := 412, "read" := true)
      )
    ,
    test("a wide record writes every member, in order"):
      val census = Census(
        "1st",
        "2nd",
        "3rd",
        "4th",
        "5th",
        "6th",
        "7th",
        "8th",
        "9th",
        "10th",
        "11th",
        "12th",
        "13th",
        "14th",
        "15th"
      )

      val expected = CirceJson.obj(
        "first" := "1st",
        "second" := "2nd",
        "third" := "3rd",
        "fourth" := "4th",
        "fifth" := "5th",
        "sixth" := "6th",
        "seventh" := "7th",
        "eighth" := "8th",
        "ninth" := "9th",
        "tenth" := "10th",
        "eleventh" := "11th",
        "twelfth" := "12th",
        "thirteenth" := "13th",
        "fourteenth" := "14th",
        "fifteenth" := "15th"
      )

      assertTrue(
        JsonCirceEncoder.encode(json.census, census) == expected,
        JsonCirceDecoder.decode(json.census, expected) == census.valid
      )
  )
