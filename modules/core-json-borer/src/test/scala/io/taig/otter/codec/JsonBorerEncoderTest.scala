package io.taig.otter.codec

import cats.syntax.all.*
import io.bullet.borer.Dom
import io.bullet.borer.Json as BorerJson
import io.taig.data.Data
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** What the encoder writes, where writing it is the claim rather than reading it back.
  *
  * [[JsonBorerRoundTripTest]] covers everything a round trip covers. What it cannot see is a document that round trips
  * but is not the one circe writes, and it cannot see the two cases where writing would *fail*: borer's JSON renderer
  * refuses a NaN and an infinity outright rather than writing something no parser would take back.
  */
object JsonBorerEncoderTest extends ZIOSpecDefault:
  private def encode[A](schema: Json.Writer[A], value: A): String =
    BorerJson.encode(value)(using JsonBorer.encoder(schema)).toUtf8String

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerEncoderTest")(
    test("a record, in declaration order"):
      assertTrue(encode(json.book, Book("Dune", 412, true)) == """{"title":"Dune","pages":412,"read":true}""")
    ,
    test("an empty record is an empty object rather than nothing"):
      assertTrue(encode(RNil, ()) == "{}")
    ,
    test("an absent optional field drops its key, or writes a null, as its annotation says"):
      assertTrue(
        encode(json.omittedTag, Note("Dune", none)) == """{"title":"Dune"}""",
        encode(json.nullableTag, Note("Dune", none)) == """{"title":"Dune","tag":null}""",
        encode(json.omittedTag, Note("Dune", 42.some)) == """{"title":"Dune","tag":42}"""
      )
    ,
    test("a collection, a tuple and a dictionary"):
      assertTrue(
        encode(collection.list(int), List(1, 2, 3)) == "[1,2,3]",
        encode(collection.list(int), Nil) == "[]",
        encode(json.printings, List(1 -> "a", 2 -> "b")) == """{"1":"a","2":"b"}""",
        encode(json.tree, Tree(1, List(Tree(2, Nil)))) ==
          """{"value":1,"children":[{"value":2,"children":[]}]}"""
      )
    ,
    test("a union writes the branch it holds, and the branch's name never reaches the document"):
      assertTrue(
        encode(json.shape, Shape.Circle(1.5)) == """{"radius":1.5}""",
        encode(json.shape, Shape.Triangle(1.5, 2.5)) == """{"base":1.5,"height":2.5}"""
      )
    ,
    /** borer's JSON renderer *fails* on a NaN and an infinity rather than writing them, so writing them as text is not
      * a nicety: without it this schema could not be written at all. circe's `fromDoubleOrString` does the same.
      */
    test("a NaN and an infinity go out as text, which is what circe writes and what borer would refuse"):
      assertTrue(
        encode(double, Double.NaN) == "\"NaN\"",
        encode(double, Double.PositiveInfinity) == "\"Infinity\"",
        encode(double, Double.NegativeInfinity) == "\"-Infinity\"",
        encode(float, Float.NaN) == "\"NaN\""
      )
    ,
    /** A `Float` is written as a `Float`, not widened to a `Double` first -- on the JVM. Scala.js has no float
      * formatting of its own: `0.1f` prints there as `0.10000000149011612` whatever writes it, circe's
      * `fromFloatOrString` included, so the two modules still agree and it is the platform that differs. The value
      * round trips either way, which is what [[JsonBorerRoundTripTest]] holds; only the text differs.
      */
    test("a float is written as a float, and a value both platforms spell the same proves it"):
      assertTrue(
        encode(float, 0.5f) == "0.5",
        encode(double, 0.1) == "0.1",
        encode(float, 0.1f) == String.valueOf(0.1f)
      )
    ,
    /** The only reason this object exists: `ConstantDecoder` and `EnumerationDecoder` ask what was *expected*, and they
      * ask it of an encoder at the decoder's own `T`. Its whole contract is that `JsonBorer.toData` renders what it
      * writes exactly as `data-circe` renders what the circe encoder writes.
      */
    test("the Dom encoder that exists only so a constant can say what it expected"):
      assertTrue(
        JsonPrimitiveBorerDomEncoder.encode(int, 412) == Dom.IntElem(412),
        JsonPrimitiveBorerDomEncoder.encode(string, "a") == Dom.StringElem("a"),
        JsonPrimitiveBorerDomEncoder.encode(boolean, true) == Dom.BooleanElem(true),
        JsonPrimitiveBorerDomEncoder.encode(double, 1.5) == Dom.DoubleElem(1.5),
        JsonPrimitiveBorerDomEncoder.encode(double, Double.NaN) == Dom.StringElem("NaN"),
        JsonBorer.toData(JsonPrimitiveBorerDomEncoder.encode(int, 412)) == 412,
        JsonBorer.toData(JsonPrimitiveBorerDomEncoder.encode(string, "a")) == ("a": Data)
      )
    ,
    test("a constant reports the value it expected, through that encoder"):
      val violations = JsonBorerDecoder
        .decode(
          field("type", constant(string, "deferred")).toRecord,
          Doc.toBorer(Doc.Obj(List("type" -> Doc.Str("x"))))
        )
        .swap
        .toOption

      assertTrue(violations.map(JsonBorer.failures(_).toList) == List(""".type: *.equals "deferred"""").some)
  )
