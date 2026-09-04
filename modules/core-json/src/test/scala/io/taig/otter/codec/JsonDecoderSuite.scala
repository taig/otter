package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import io.taig.validation.Violation
import zio.Scope
import zio.test.*

import java.util.UUID
import scala.collection.immutable.SortedMap

/** What every JSON interpreter reads, stated as the answer the alphabet says a document has.
  *
  * A document is written as text, the same form [[JsonEncoderSuite]] asserts against, so the two directions of the
  * contract speak one language. See [[JsonInterpreter]] for why text is the boundary and what it means that reading it
  * goes through the interpreter's own parser.
  *
  * A differential test between two interpreters is the stronger instrument where there are two -- it compares whole
  * violation trees over a corpus no one would write out by hand -- but it says only that they are the same, and it
  * needs one of them to be the oracle. This is the other half: the written answer, which an interpreter that has no
  * oracle can still be held to.
  *
  * A good part of what it asserts -- how two layers of absence are told apart, what leniency does -- is a claim about
  * `core`'s combinators, which every interpreter runs. That is worth knowing about it: its value is not that it might
  * catch the second implementation, but that it is the answer the third one is measured against.
  *
  * A duplicated key appears nowhere below, and that is deliberate rather than an oversight: JSON does not say which
  * occurrence wins, the two interpreters here genuinely disagree, and a contract with a knob per disagreement would
  * have stopped asserting anything. Each module states its own behaviour instead.
  */
abstract class JsonDecoderSuite(interpreter: JsonInterpreter) extends ZIOSpecDefault:
  /** What this interpreter reads that the contract cannot ask about. Empty is the whole of it. */
  protected def extra: List[Spec[TestEnvironment & Scope, Any]] = Nil

  private def decode[A](schema: Json.Reader[A], document: String): Validated[Violations, A] =
    interpreter.decode(schema, document)

  private def constraints[A](result: Validated[Violations, A]): List[Constraint] =
    result.fold(violations.constraints, _ => Nil)

  private def paths[A](result: Validated[Violations, A]): List[List[Step]] =
    result.fold(violations.paths, _ => Nil)

  private val contract: Spec[TestEnvironment & Scope, Any] = suite("contract")(
    test("Json.Primitive"):
      assertTrue(
        decode(string, "\"foobar\"") == "foobar".valid,
        decode(int, "42") == 42.valid,
        decode(boolean, "true") == true.valid
      )
    ,
    test("Json.Primitive: uuid"):
      val id = UUID.fromString("1c1a5f8e-6e33-4e34-8d2e-3f8b2f0e1a2b")
      assertTrue(
        decode(uuid, "\"" + id.toString + "\"") == id.valid,
        decode(uuid, "\"not-a-uuid\"").isInvalid
      )
    ,
    test("Json.Primitive: locale"):
      val value = java.util.Locale.forLanguageTag("en-US")
      assertTrue(
        decode(locale, "\"en-US\"") == value.valid,
        decode(locale, "\"not a locale\"").isInvalid
      )
    ,
    test("Json.Primitive: currency"):
      val value = java.util.Currency.getInstance("USD")
      assertTrue(
        decode(currency, "\"USD\"") == value.valid,
        decode(currency, "\"not-a-currency\"").isInvalid
      )
    ,
    test("Json.Primitive: uri"):
      val value = new java.net.URI("https://example.com/path")
      assertTrue(
        decode(uri, "\"https://example.com/path\"") == value.valid,
        decode(uri, "\"not a uri\"").isInvalid
      )
    ,
    test("Json.Primitive: charset"):
      val value = java.nio.charset.Charset.forName("UTF-8")
      assertTrue(
        decode(charset, "\"utf8\"") == value.valid,
        decode(charset, "\"not a charset\"").isInvalid
      )
    ,
    test("Json.Primitive: regex"):
      val value = java.util.regex.Pattern.compile("a+b*")
      assertTrue(
        decode(regex, """"a+b*"""").map(_.pattern) == value.pattern.valid,
        decode(regex, """"("""").isInvalid
      )
    ,
    test("Json.Primitive: type mismatch"):
      val expected = Violations(
        Violation(constraint = Constraint.Generic.Type(name = "int"), actual = "string", hint = none)
      ).invalid

      assertTrue(decode(int, "\"foobar\"") == expected)
    ,
    test("Json.Coerce: accepts the laxer representation"):
      assertTrue(
        decode(coerce(boolean), "\"true\"") == true.valid,
        decode(coerce(boolean), "false") == false.valid,
        decode(coerce(int), "\"42\"") == 42.valid,
        decode(coerce(int), "42") == 42.valid,
        decode(coerce(string), "42") == "42".valid
      )
    ,
    test("Json.Coerce: still rejects what it cannot read"):
      assertTrue(decode(coerce(boolean), "42").isInvalid)
    ,
    test("Json.Collection"):
      assertTrue(decode(collection.list(string), """["foo","bar"]""") == List("foo", "bar").valid)
    ,
    test("Json.Dictionary"):
      assertTrue(decode(dictionary.list(string), """{"foo":"1","bar":"2"}""") == List("foo" -> "1", "bar" -> "2").valid)
    ,
    test("Json.Dictionary: a map collects its entries by key"):
      assertTrue(
        decode(dictionary.map(string), """{"foo":"1","bar":"2"}""") == SortedMap("bar" -> "2", "foo" -> "1").valid
      )
    ,
    test("Json.Dictionary: a typed key is parsed by its schema"):
      val id = UUID.fromString("6b1a4a5c-3a1e-4f0e-9b7e-2f0f5b3c9a11")
      assertTrue(decode(json.editions, "{\"" + id.toString + "\":3}") == SortedMap(id -> 3).valid)
    ,
    test("Json.Dictionary: an integer key is read out of the text it is written as"):
      assertTrue(decode(json.printings, """{"5":"first"}""") == List(5 -> "first").valid)
    ,
    test("Json.Dictionary: a key that cannot be parsed is reported under the text the document holds"):
      val result = decode(json.editions, """{"nope":3}""")

      assertTrue(
        paths(result) == List(List(Step.Field("nope"))),
        constraints(result) == List(Constraint.Generic.Type("uuid"))
      )
    ,
    test("Json.Dictionary: a bad key and a bad value in the same entry are reported together"):
      val result = decode(json.editions, """{"nope":"three"}""")

      assertTrue(constraints(result) == List(Constraint.Generic.Type("uuid"), Constraint.Generic.Type("int")))
    ,
    test("Json.Record"):
      val schema = field("foo", string) :* field("bar", int) :* field("baz", boolean)
      assertTrue(decode(schema, """{"foo":"John Doe","bar":42,"baz":true}""") == ("John Doe", 42, true).valid)
    ,
    test("Json.Record: optional field"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(
        decode(schema, """{"foo":"x","bar":42}""") == ("x", 42.some).valid,
        decode(schema, """{"foo":"x"}""") == ("x", none).valid
      )
    ,
    test("Json.Record: an optional field accepts an explicit null"):
      val schema = field("foo", string) :* field("bar", int).optional
      assertTrue(decode(schema, """{"foo":"x","bar":null}""") == ("x", none).valid)
    ,
    test("Json.Record: a defaulted field accepts an explicit null"):
      val schema = field("bar", int).optional(7).toRecord
      assertTrue(
        decode(schema, """{"bar":null}""") == 7.valid,
        decode(schema, "{}") == 7.valid,
        decode(schema, """{"bar":1}""") == 1.valid
      )
    ,
    test("Json.Record: a required field still rejects null"):
      val schema = field("bar", int).toRecord
      val result = decode(schema, """{"bar":null}""")

      assertTrue(
        result.isInvalid,
        paths(result) == List(List(Step.Field("bar"))),
        constraints(result) == List(Constraint.Generic.Type("int"))
      )
    ,
    test("Json.Record: a field holding an optional schema wants its key"):
      val schema = field("bar", int.optional).toRecord
      assertTrue(
        decode(schema, """{"bar":null}""") == none.valid,
        decode(schema, """{"bar":1}""") == 1.some.valid,
        decode(schema, "{}").isInvalid
      )
    ,
    test("Json.Record: a strict omitted field rejects a null"):
      val schema = field("bar", int).optional.omitted.strict.toRecord
      assertTrue(
        decode(schema, "{}") == none.valid,
        decode(schema, """{"bar":1}""") == 1.some.valid,
        decode(schema, """{"bar":null}""").isInvalid
      )
    ,
    test("Json.Record: a strict nullable field rejects a missing key"):
      val schema = field("bar", int).optional.nullable.strict.toRecord
      assertTrue(
        decode(schema, """{"bar":null}""") == none.valid,
        decode(schema, """{"bar":1}""") == 1.some.valid,
        decode(schema, "{}").isInvalid
      )
    ,
    test("Json.Record: leniency is what reading does anyway"):
      val nullable = field("bar", int).optional.nullable.toRecord
      val omitted = field("bar", int).optional.omitted.toRecord
      assertTrue(decode(nullable, """{"bar":null}""") == decode(omitted, """{"bar":null}"""))
    ,
    test("Json.Record: only a strict field tells two layers of absence apart"):
      val strict = field("bar", int.optional).optional.omitted.strict.toRecord
      val lenient = field("bar", int.optional).optional.toRecord
      assertTrue(
        decode(strict, "{}") == none.valid,
        decode(strict, """{"bar":null}""") == none.some.valid,
        decode(strict, """{"bar":1}""") == 1.some.some.valid,
        decode(lenient, """{"bar":null}""") == none.valid
      )
    ,
    test("Json.Record: missing field"):
      val schema = field("foo", string) :* field("bar", int)
      assertTrue(decode(schema, """{"foo":"x"}""").isInvalid)
    ,
    test("Json.Tuple"):
      val schema = TNil :* string :* int :* boolean
      assertTrue(decode(schema, """["John Doe",42,true]""") == ("John Doe", 42, true).valid)
    ,
    test("Json.Tuple: wrong arity"):
      val schema = TNil :* string :* int
      assertTrue(decode(schema, """["x"]""").isInvalid)
    ,
    test("Json.Constant"):
      assertTrue(
        decode(constant(string, "foobar"), "\"foobar\"") == ().valid,
        decode(constant(string, "foobar"), "\"barfoo\"").isInvalid
      )
    ,
    test("Json.Record: case class"):
      assertTrue(decode(json.book, """{"title":"Dune","pages":412,"read":true}""") == Book("Dune", 412, true).valid)
    ,
    test("Json.Union: enum"):
      assertTrue(
        decode(json.shape, """{"radius":1.5}""") == Shape.Circle(1.5).valid,
        decode(json.shape, """{"side":2.0}""") == Shape.Square(2.0).valid,
        decode(json.shape, """{"base":3.0,"height":4.0}""") == Shape.Triangle(3.0, 4.0).valid
      )
    ,
    test("Json.Union: branches that all read the same enum"):
      assertTrue(
        decode(json.verdict, """{"type":"accepted"}""") == Verdict.Accepted.valid,
        decode(json.verdict, """{"type":"rejected"}""") == Verdict.Rejected.valid,
        decode(json.verdict, """{"type":"deferred","reason":"late"}""") == Verdict.Deferred("late").valid,
        decode(json.verdict, """{"type":"withdrawn"}""").isInvalid
      )
    ,
    test("violations carry the path to the failure"):
      val schema = field("foo", string) :* field("bar", collection.list(int))

      assertTrue(
        paths(decode(schema, """{"foo":"x","bar":[1,"nope"]}""")) ==
          List(List(Step.Field("bar"), Step.Index(1)))
      )
    ,
    test("Json.Enumeration"):
      assertTrue(
        decode(json.genre, "\"history\"") == Genre.History.valid,
        decode(json.genre, "\"nope\"").isInvalid
      )
    ,
    test("read only schema"):
      assertTrue(decode(json.isbn, "\"978\"") == Isbn("978").valid)
  )

  final override def spec: Spec[TestEnvironment & Scope, Any] =
    suite(interpreter.name + "DecoderTest")((contract :: extra)*)
