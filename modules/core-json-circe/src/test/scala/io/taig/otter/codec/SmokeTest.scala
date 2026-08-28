package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import zio.Scope
import zio.test.*

object SmokeTest extends ZIOSpecDefault:
  val record: Json.Record[(String, Int, Boolean), (String, Int, Boolean)] =
    field("title", string) :* field("pages", int) :* field("read", boolean)

  val book: Json.Record[Book, Book] = record.to[Book]

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SmokeTest")(
    test("round trip"):
      val json = JsonCirceEncoder.encode(book, Book("Dune", 412, true))
      assertTrue(JsonCirceDecoder.decode(book, json) == cats.data.Validated.valid(Book("Dune", 412, true)))
  )
