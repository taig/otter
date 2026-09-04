package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

import java.util.UUID
import scala.collection.immutable.SortedMap

/** What every JSON interpreter writes, stated as the text it writes.
  *
  * Text rather than a document because an interpreter is not obliged to build one: `core-json-borer`'s encoder carries
  * a deferred write and never assembles a value at all, so the wire is the only place two of them meet. It is also the
  * only form in which they can be compared -- an assertion against `io.circe.Json` says something no other interpreter
  * can be held to, which is exactly why the borer suite could not mirror the circe one and wrote its own literals
  * instead.
  *
  * What is left out is left out on purpose. How a number is spelled is the platform's business rather than the
  * alphabet's -- Scala.js has no float formatting of its own, so `0.1f` prints there as `0.10000000149011612` whatever
  * writes it, and an integral `Double` is `2.0` on the JVM and `2` there -- so the fixtures below avoid the values
  * where the two platforms differ, and an interpreter states its own spelling in its own suite. Whether a renderer
  * refuses a NaN outright is a fact about a library rather than about the alphabet, and belongs there too.
  */
abstract class JsonEncoderSuite(interpreter: JsonInterpreter) extends ZIOSpecDefault:
  /** What this interpreter writes that the contract cannot ask about. Empty is the whole of it. */
  protected def extra: List[Spec[TestEnvironment & Scope, Any]] = Nil

  private def encode[A](schema: Json.Writer[A], value: A): String = interpreter.encode(schema, value)

  private val contract: Spec[TestEnvironment & Scope, Any] = suite("contract")(
    test("Json.Primitive"):
      assertTrue(
        encode(string, "foobar") == "\"foobar\"",
        encode(int, 42) == "42",
        encode(boolean, true) == "true"
      )
    ,
    test("Json.Primitive: uuid"):
      val id = UUID.fromString("1c1a5f8e-6e33-4e34-8d2e-3f8b2f0e1a2b")
      assertTrue(encode(uuid, id) == "\"" + id.toString + "\"")
    ,
    test("Json.Constant"):
      assertTrue(encode(constant(string, "foobar"), ()) == "\"foobar\"")
    ,
    test("Json.Coerce"):
      assertTrue(encode(coerce(boolean), true) == "true")
    ,
    test("Json.Collection"):
      assertTrue(
        encode(collection.list(string), List("foo", "bar")) == """["foo","bar"]""",
        encode(collection.list(string), Nil) == "[]"
      )
    ,
    test("Json.Dictionary"):
      assertTrue(encode(dictionary.list(string), List("foo" -> "1", "bar" -> "2")) == """{"foo":"1","bar":"2"}""")
    ,
    test("Json.Dictionary: a map is written in key order"):
      assertTrue(
        encode(dictionary.map(string), SortedMap("foo" -> "1", "bar" -> "2")) == """{"bar":"2","foo":"1"}"""
      )
    ,
    test("Json.Dictionary: a typed key is printed by its schema"):
      val id = UUID.fromString("6b1a4a5c-3a1e-4f0e-9b7e-2f0f5b3c9a11")
      assertTrue(encode(json.editions, SortedMap(id -> 3)) == "{\"" + id.toString + "\":3}")
    ,
    test("Json.Dictionary: an integer key is written as the text it is"):
      assertTrue(encode(json.printings, List(5 -> "first")) == """{"5":"first"}""")
    ,
    test("Json.Branch: a name spelled as a value is the name it prints to"):
      val value = Shape.Circle(1.5)
      assertTrue(encode(json.taggedShape, value) == encode(json.shape, value))
    ,
    test("Json.Record"):
      val schema = field("foo", string) :* field("bar", int) :* field("baz", boolean)
      assertTrue(encode(schema, ("John Doe", 42, true)) == """{"foo":"John Doe","bar":42,"baz":true}""")
    ,
    test("Json.Record: optional field"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(
        encode(schema, ("John Doe", 42.some)) == """{"foo":"John Doe","bar":42}""",
        encode(schema, ("John Doe", none)) == """{"foo":"John Doe"}"""
      )
    ,
    test("Json.Record: nullable optional field"):
      val schema = field("foo", string) :* field("bar", int).optional.nullable
      assertTrue(
        encode(schema, ("John Doe", 42.some)) == """{"foo":"John Doe","bar":42}""",
        encode(schema, ("John Doe", none)) == """{"foo":"John Doe","bar":null}"""
      )
    ,
    test("Json.Record: omitting is what a field does anyway"):
      val implicitly = field("foo", string) :* field("bar", int).optional
      val explicitly = field("foo", string) :* field("bar", int).optional.omitted
      assertTrue(encode(implicitly, ("John Doe", none)) == encode(explicitly, ("John Doe", none)))
    ,
    test("Json.Record: the attribute survives .optional"):
      val before = field("bar", int).nullable.optional.toRecord
      val after = field("bar", int).optional.nullable.toRecord
      assertTrue(
        encode(before, none) == """{"bar":null}""",
        encode(after, none) == """{"bar":null}"""
      )
    ,
    test("Json.Record: a globally set attribute is read"):
      val schema = field("bar", int).optional.attr(Keys.absence, Absence.Empty).toRecord
      assertTrue(encode(schema, none) == """{"bar":null}""")
    ,
    test("Json.Record: the json namespace wins over the global one"):
      val schema = field("bar", int).optional
        .attr(Keys.absence, Absence.Empty)
        .attr(Json.Namespace, Keys.absence, Absence.Omit)
        .toRecord
      assertTrue(encode(schema, none) == "{}")
    ,
    test("Json.Record: a defaulted field writes whatever it holds"):
      val schema = field("bar", int).optional(0).nullable.toRecord
      assertTrue(
        encode(schema, 42) == """{"bar":42}""",
        encode(schema, 0) == """{"bar":0}"""
      )
    ,
    test("Json.Record: RNil is an empty object rather than nothing"):
      assertTrue(encode(RNil, ()) == "{}")
    ,
    test("Json.Tuple"):
      val schema = TNil :* string :* int :* boolean
      assertTrue(encode(schema, ("John Doe", 42, true)) == """["John Doe",42,true]""")
    ,
    test("Json.Tuple: TNil"):
      assertTrue(encode(TNil, ()) == "[]")
    ,
    test("Json.Record: case class"):
      assertTrue(encode(json.book, Book("Dune", 412, true)) == """{"title":"Dune","pages":412,"read":true}""")
    ,
    /** Every double here is written with a fractional part on purpose: an integral one is spelled `2.0` on the JVM and
      * `2` on Scala.js, whatever writes it, and this test is about which branch a union writes rather than about how a
      * number is spelled.
      */
    test("Json.Union: enum"):
      assertTrue(
        encode(json.shape, Shape.Circle(1.5)) == """{"radius":1.5}""",
        encode(json.shape, Shape.Square(2.5)) == """{"side":2.5}""",
        encode(json.shape, Shape.Triangle(3.5, 4.5)) == """{"base":3.5,"height":4.5}"""
      )
    ,
    test("recursive schema"):
      val tree = Tree(1, List(Tree(2, Nil), Tree(3, Nil)))
      assertTrue(
        encode(json.tree, tree) ==
          """{"value":1,"children":[{"value":2,"children":[]},{"value":3,"children":[]}]}"""
      )
    ,
    test("Json.Enumeration"):
      assertTrue(
        encode(json.genre, Genre.Fiction) == "\"fiction\"",
        encode(json.genre, Genre.Poetry) == "\"poetry\""
      )
    ,
    test("write only schema"):
      assertTrue(encode(json.title, Book("Dune", 412, true)) == "\"Dune\"")
    ,
    test("a member that only writes leaves the members after it where they are"):
      assertTrue(
        encode(json.shelf, Shelf(Isbn("978"), 412, true)) == """{"label":"978","pages":412,"read":true}"""
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

      val expected = """{"first":"1st","second":"2nd","third":"3rd","fourth":"4th","fifth":"5th","sixth":"6th",""" +
        """"seventh":"7th","eighth":"8th","ninth":"9th","tenth":"10th","eleventh":"11th","twelfth":"12th",""" +
        """"thirteenth":"13th","fourteenth":"14th","fifteenth":"15th"}"""

      assertTrue(encode(json.census, census) == expected)
  )

  final override def spec: Spec[TestEnvironment & Scope, Any] =
    suite(interpreter.name + "EncoderTest")((contract :: extra)*)
