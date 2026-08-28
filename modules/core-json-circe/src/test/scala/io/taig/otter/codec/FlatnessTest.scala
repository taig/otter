package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** A node carries the type of what is inside it, so a schema a flat format cannot represent is rejected by the compiler
  * rather than at conversion time.
  */
object FlatnessTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("FlatnessTest")(
    test("a record whose fields are all primitives is flat"):
      assertTrue(typeChecks("""val schema: Json.Record.Flat[Book] =
        (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]"""))
    ,
    test("a record holding a record is not flat"):
      assertTrue(!typeChecks("""val schema: Json.Record.Flat[(Book, Int)] =
        field("book", json.flatBook) :* field("pages", int)"""))
    ,
    test("a record holding a collection is not flat"):
      assertTrue(!typeChecks("""val schema: Json.Record.Flat[(List[String], Int)] =
        field("tags", collection.list(string)) :* field("pages", int)"""))
    ,
    test("a flat record is still an ordinary schema"):
      assertTrue(typeChecks("""val schema: Json.Record[Book] = json.flatBook"""))
    ,
    test("a flat record still encodes and decodes"):
      assertTrue(
        typeChecks("""JsonCirceEncoder.encode(json.flatBook, Book("a", 1, true))"""),
        typeChecks("""JsonCirceDecoder.decode(json.flatBook, io.circe.Json.Null)""")
      )
  )
