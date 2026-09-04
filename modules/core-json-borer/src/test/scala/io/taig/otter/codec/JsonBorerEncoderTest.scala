package io.taig.otter.codec

import io.bullet.borer.Dom
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** What borer writes that [[JsonEncoderSuite]] cannot ask about.
  *
  * The contract states what the alphabet says a document looks like, and this module answers it like any other. What is
  * left is what only borer has: a renderer that *refuses* a NaN and an infinity outright rather than writing something
  * no parser would take back, a `Float` spelling that is the platform's rather than the alphabet's, and a `Dom` encoder
  * that exists only so a constant can say what it expected.
  */
object JsonBorerEncoderTest extends JsonEncoderSuite(JsonBorerInterpreter):
  private def encode[A](schema: Json.Writer[A], value: A): String = JsonBorerInterpreter.encode(schema, value)

  override protected val extra: List[Spec[TestEnvironment & Scope, Any]] = List(
    suite("borer")(
      /** borer's JSON renderer *fails* on a NaN and an infinity rather than writing them, so writing them as text is
        * not a nicety: without it this schema could not be written at all. circe's `fromDoubleOrString` does the same,
        * but that is circe's choice rather than the alphabet's, so it is stated here rather than in the contract.
        */
      test("a NaN and an infinity go out as text, which is what borer would otherwise refuse"):
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
        * round trips either way, which is what the contract's round trip holds; only the text differs.
        */
      test("a float is written as a float, and a value both platforms spell the same proves it"):
        assertTrue(
          encode(float, 0.5f) == "0.5",
          encode(double, 0.1) == "0.1",
          encode(float, 0.1f) == String.valueOf(0.1f)
        )
      ,
      /** The only reason this object exists: `ConstantDecoder` and `EnumerationDecoder` ask what was *expected*, and
        * they ask it of an encoder at the decoder's own `T`. Its whole contract is that `JsonBorer.toData` renders what
        * it writes exactly as `data-circe` renders what the circe encoder writes.
        */
      test("the Dom encoder that exists only so a constant can say what it expected"):
        assertTrue(
          JsonPrimitiveBorerDomEncoder.encode(int, 412) == Dom.IntElem(412),
          JsonPrimitiveBorerDomEncoder.encode(string, "a") == Dom.StringElem("a"),
          JsonPrimitiveBorerDomEncoder.encode(boolean, true) == Dom.BooleanElem(true),
          JsonPrimitiveBorerDomEncoder.encode(double, 1.5) == Dom.DoubleElem(1.5),
          JsonPrimitiveBorerDomEncoder.encode(double, Double.NaN) == Dom.StringElem("NaN"),
          JsonBorer.toData(JsonPrimitiveBorerDomEncoder.encode(int, 412)) == 412,
          JsonBorer.toData(JsonPrimitiveBorerDomEncoder.encode(string, "a")) == ("a": io.taig.data.Data)
        )
      ,
      test("a constant reports the value it expected, through that encoder"):
        val violations = JsonBorerDecoder
          .decode(
            field("type", constant(string, "deferred")).toRecord,
            BorerDoc.toBorer(Doc.Obj(List("type" -> Doc.Str("x"))))
          )
          .swap
          .toOption

        assertTrue(violations.map(JsonBorer.failures(_).toList) == Some(List(""".type: *.equals "deferred"""")))
    )
  )
