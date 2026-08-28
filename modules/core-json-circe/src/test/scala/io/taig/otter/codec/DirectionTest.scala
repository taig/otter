package io.taig.otter.codec

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
  )
