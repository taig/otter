package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Dom
import io.bullet.borer.Tag
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.JsonBorer
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** What [[JsonBorerAgreementTest]] structurally cannot reach.
  *
  * The agreement test hands borer *bytes*, so everything it exercises is a `Dom` borer's own JSON parser produced --
  * which is only ever the unsized array and map, and never a byte string, a tag or a non string key. A `Dom` built by
  * hand, or one read from CBOR, can hold all of those, and this module has to answer for them. Every fixture below is
  * therefore built rather than parsed, and every answer is stated absolutely, because there is no circe document that
  * corresponds to it to agree with.
  */
object JsonBorerDecoderTest extends ZIOSpecDefault:
  private def constraints(violations: Violations): List[Constraint] = violations match
    case Violations.Root(values, found) =>
      found.toList.map(_.constraint) ++ values.toList.flatMap((_, nested) => constraints(nested))
    case Violations.Namespace(values) => values.toSortedMap.toList.flatMap((_, nested) => constraints(nested))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerDecoderTest")(
    test("a sized map reads as an object, which is the form CBOR produces"):
      val element = Dom.MapElem.Sized(
        "title" -> Dom.StringElem("Dune"),
        "pages" -> Dom.IntElem(412),
        "read" -> Dom.BooleanElem(true)
      )

      assertTrue(JsonBorerDecoder.decode(json.book, element) == Validated.valid(Book("Dune", 412, true)))
    ,
    test("a sized array reads as an array"):
      val element = Dom.ArrayElem.Sized(Dom.IntElem(1), Dom.IntElem(2))

      assertTrue(JsonBorerDecoder.decode(collection.list(int), element) == Validated.valid(List(1, 2)))
    ,
    /** `Dom.MapElem.stringKeyedMembers` would have dropped this member silently, which is why the interpreter uses
      * `members` and rejects the key instead.
      */
    test("a member whose key is not text is reported rather than dropped"):
      val element = Dom.MapElem.Unsized(Dom.IntElem(1) -> Dom.StringElem("a"))
      val violations = JsonBorerDecoder.decode(json.printings, element).swap.toOption

      assertTrue(violations.map(constraints) == List[Constraint](Constraint.Generic.Type("string")).some)
    ,
    test("a text stream reads as the string it compacts to"):
      val element = Dom.TextStreamElem(Vector(Dom.StringElem("Du"), Dom.StringElem("ne")))

      assertTrue(JsonBorerDecoder.decode(string, element) == Validated.valid("Dune"))
    ,
    test("a NaN reaches a double schema as a NaN, because a hand built Dom can hold one"):
      assertTrue(JsonBorerDecoder.decode(double, Dom.DoubleElem(Double.NaN)).exists(_.isNaN))
    ,
    test("an element JSON has no counterpart for is a type mismatch, not a crash"):
      val elements = List(
        Dom.UndefinedElem,
        Dom.ByteArrayElem(Array[Byte](1, 2)),
        Dom.TaggedElem(Tag.EpochDateTime, Dom.IntElem(1)),
        Dom.OverLongElem(negative = false, value = 1L)
      )

      assertTrue(elements.forall(element => JsonBorerDecoder.decode(json.book, element).isInvalid))
    ,
    test("typeOf names what arrived, which is what a mismatch is reported against"):
      assertTrue(
        JsonBorer.typeOf(Dom.NullElem) == "null",
        JsonBorer.typeOf(Dom.BooleanElem(true)) == "boolean",
        JsonBorer.typeOf(Dom.IntElem(1)) == "number",
        JsonBorer.typeOf(Dom.NumberStringElem("1e400")) == "number",
        JsonBorer.typeOf(Dom.StringElem("a")) == "string",
        JsonBorer.typeOf(Dom.TextStreamElem(Vector.empty)) == "string",
        JsonBorer.typeOf(Dom.ArrayElem.Sized(Dom.NullElem)) == "array",
        JsonBorer.typeOf(Dom.MapElem.Sized("a" -> Dom.NullElem)) == "object",
        JsonBorer.typeOf(Dom.ByteArrayElem(Array.empty)) == "bytes",
        JsonBorer.typeOf(Dom.UndefinedElem) == "undefined",
        JsonBorer.typeOf(Dom.TaggedElem(Tag.EpochDateTime, Dom.NullElem)) == "unknown"
      )
    ,
    /** Every violation carries what arrived as a `Data`, and both modules compare those with universal equality, so the
      * ladder has to land on the same rung circe's does. `1.50` reaching `Float` rather than `BigDecimal` is why a non
      * canonical decimal still agrees.
      */
    test("toData lands on the same rung data-circe lands on"):
      assertTrue(
        JsonBorer.toData(Dom.NullElem) == Data.Null,
        JsonBorer.toData(Dom.BooleanElem(true)) == true,
        JsonBorer.toData(Dom.IntElem(412)) == 412,
        JsonBorer.toData(Dom.LongElem(412L)) == 412,
        JsonBorer.toData(Dom.DoubleElem(1.5)) == 1.5f,
        JsonBorer.toData(Dom.NumberStringElem("1.50")) == 1.5f,
        JsonBorer.toData(Dom.StringElem("a")) == "a",
        JsonBorer.toData(Dom.ArrayElem.Sized(Dom.IntElem(1))) == Data.Array(List(1)),
        JsonBorer.toData(Dom.MapElem.Sized("a" -> Dom.IntElem(1))) == Data.Object(List("a" -> 1))
      )
    ,
    test("a coercion takes the laxer form, and a number written oddly is the one it cannot spell circe's way"):
      assertTrue(
        JsonBorerDecoder.decode(coerce(int), Dom.StringElem("412")) == Validated.valid(412),
        JsonBorerDecoder.decode(coerce(boolean), Dom.StringElem("true")) == Validated.valid(true),
        JsonBorerDecoder.decode(coerce(string), Dom.IntElem(412)) == Validated.valid("412"),
        JsonBorerDecoder.decode(coerce(string), Dom.BooleanElem(true)) == Validated.valid("true"),
        // Not a JSON number, so nothing is coerced and the mismatch stands.
        JsonBorerDecoder.decode(coerce(int), Dom.StringElem("+412")).isInvalid,
        JsonBorerDecoder.decode(coerce(int), Dom.StringElem("0x1")).isInvalid
      )
  )
