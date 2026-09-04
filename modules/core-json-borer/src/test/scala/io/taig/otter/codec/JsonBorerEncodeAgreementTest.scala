package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** [[JsonBorerAgreementTest]], in the direction it does not go.
  *
  * That test reads: it says a document read through borer is the answer circe reads. Nothing said the same about
  * writing, and neither of the two instruments that look like they would can see it. A round trip cannot: a document
  * that round trips is not necessarily the one circe writes. The contract cannot either: it states what the alphabet
  * says a document looks like, over the handful of values a person wrote down.
  *
  * So this is the write side stated the same way the read side is -- over generated values, differentially, with circe
  * as the witness. It is also the form that survives Scala.js, where an absolute assertion over a `Float` cannot go:
  * both libraries print the widened value there, so they still agree even where the JVM's text is not the platform's.
  */
object JsonBorerEncodeAgreementTest extends ZIOSpecDefault:
  private def agrees[A](schema: Json.Writer[A], value: A): TestResult =
    assertTrue(JsonBorerInterpreter.encode(schema, value) == JsonCirceInterpreter.encode(schema, value))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerEncodeAgreementTest")(
    test("case class"):
      check(gen.book)(agrees(json.book, _))
    ,
    test("enum through a union"):
      check(gen.shape)(agrees(json.shape, _))
    ,
    test("enum through a union whose branches read the same type"):
      check(gen.verdict)(agrees(json.verdict, _))
    ,
    test("enum through a union whose branches are named by a value"):
      check(gen.shape)(agrees(json.taggedShape, _))
    ,
    test("optional field, omitted"):
      check(gen.note)(agrees(json.omittedTag, _))
    ,
    test("optional field, nullable"):
      check(gen.note)(agrees(json.nullableTag, _))
    ,
    test("two layers of absence, kept apart by a strict field"):
      check(Gen.option(Gen.option(Gen.int)))(agrees(json.nestedTag, _))
    ,
    test("enumeration"):
      check(gen.genre)(agrees(json.genre, _))
    ,
    test("recursive schema"):
      check(gen.tree(depth = 3))(agrees(json.tree, _))
    ,
    test("dictionary with a typed key"):
      check(gen.editions)(agrees(json.editions, _))
    ,
    test("dictionary with an integer key, which the document holds as text"):
      check(gen.printings)(agrees(json.printings, _))
    ,
    test("a 15 field record"):
      check(gen.census)(agrees(json.census, _))
    ,
    /** A `Double` is where the two renderers have the most room to differ, and where a difference would be invisible to
      * a round trip: both would read their own text back to the same value.
      */
    test("a double, whatever it is"):
      check(Gen.double)(agrees(double, _))
    ,
    test("a string, whatever is in it"):
      check(Gen.string)(agrees(string, _))
  )
