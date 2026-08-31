package io.taig.otter.codec

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Validated
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.Isbn
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** The point of carrying the direction in the type is that the compiler rejects the wrong one. */
object DirectionTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("DirectionTest")(
    test("a write only schema can be encoded"):
      assertTrue(typeChecks("""JsonCirceEncoder.encode(json.title, io.taig.otter.fixture.Book("a", 1, true))"""))
    ,
    test("a write only schema cannot be decoded"):
      assertTrue(!typeChecks("""val isbn: io.taig.otter.fixture.Isbn =
        JsonCirceDecoder.decode(json.title, io.circe.Json.Null).getOrElse(???)"""))
    ,
    test("a read only schema can be decoded"):
      assertTrue(typeChecks("""JsonCirceDecoder.decode(json.isbn, io.circe.Json.fromString("978"))"""))
    ,
    test("a read only schema cannot be encoded"):
      assertTrue(!typeChecks("""JsonCirceEncoder.encode(json.isbn, io.taig.otter.fixture.Isbn("978"))"""))
    ,
    test("a round tripping schema works in both directions"):
      assertTrue(
        typeChecks("""JsonCirceEncoder.encode(json.book, io.taig.otter.fixture.Book("a", 1, true))"""),
        typeChecks("""JsonCirceDecoder.decode(json.book, io.circe.Json.Null)""")
      )
    ,
    test("map leaves a reader, not a round trip"):
      assertTrue(
        typeChecks("""val schema: Json.Primitive.Text.Reader[String] = string.map(_.reverse)"""),
        !typeChecks("""val schema: Json.Primitive.Text[String] = string.map(_.reverse)""")
      )
    ,
    test("a mapped schema cannot be encoded"):
      assertTrue(!typeChecks("""JsonCirceEncoder.encode(string.map(_.reverse), "a")"""))
    ,
    test("contramap leaves a writer, not a round trip"):
      assertTrue(
        typeChecks("""val schema: Json.Primitive.Text.Writer[Book] = string.contramap[Book](_.title)"""),
        !typeChecks("""val schema: Json.Primitive.Text[Book] = string.contramap[Book](_.title)""")
      )
    ,
    test("a contramapped schema cannot be decoded"):
      assertTrue(!typeChecks("""val title: String =
        JsonCirceDecoder.decode(string.contramap[Book](_.title), io.circe.Json.Null).getOrElse(???)"""))
    ,
    test("a reader is a Functor"):
      val schema = Functor[[a] =>> Json.Primitive.Text.Schema[Nothing, a]].map(json.isbn)(_.value)
      assertTrue(JsonCirceDecoder.decode(schema, CirceJson.fromString("978")) == Validated.valid("978"))
    ,
    test("a writer is a Contravariant"):
      val schema = Contravariant[[a] =>> Json.Primitive.Text.Schema[a, Any]]
        .contramap(json.title)((isbn: Isbn) => Book(isbn.value, 1, true))
      assertTrue(JsonCirceEncoder.encode(schema, Isbn("978")) == CirceJson.fromString("978"))
    ,
    test("a round tripping schema is an Invariant"):
      val schema = Invariant[[a] =>> Json.Record.Schema[Json.Node, a, a]]
        .imap(json.book)(_.title)(Book(_, 1, true))
      val encoded = JsonCirceEncoder.encode(schema, "Dune")
      assertTrue(JsonCirceDecoder.decode(schema, encoded) == Validated.valid("Dune"))
  )
