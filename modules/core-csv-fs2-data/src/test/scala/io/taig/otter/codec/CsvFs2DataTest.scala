package io.taig.otter.codec

import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.Fallible
import fs2.Pure
import fs2.Stream
import fs2.data.csv.lowlevel
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** The boundary with fs2-data, which owns the text the way circe owns it for JSON. */
object CsvFs2DataTest extends ZIOSpecDefault:
  /** A title that has to be quoted and has a quote of its own to escape. */
  private val book = Book("""Herbert, Frank: "Dune"""", 412, true)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvFs2DataTest")(
    test("a keyed row carries its headers"):
      val row = CsvKeyedRowEncoder.encode(csv.book, book)
      assertTrue(
        row.map(_.headers.value.toList) == List("title", "pages", "read").some,
        row.map(_.values.toList) == List(book.title, "412", "true").some
      )
    ,
    test("a keyed row round trips"):
      val row = CsvKeyedRowEncoder.encode(csv.book, book)
      assertTrue(row.map(CsvKeyedRowDecoder.decode(csv.book, _)) == book.valid.some)
    ,
    test("a positional row round trips"):
      val row = CsvRowEncoder.encode(csv.positional, book)
      assertTrue(row.map(CsvRowDecoder.decode(csv.positional, _)) == book.valid.some)
    ,
    test("a keyed row is aligned to its schema's header, so a dropped column comes back empty"):
      val row = CsvKeyedRowEncoder.encode(csv.omittedTag, Note("Dune", none))
      val cells = CsvRecordEncoder.encode(csv.omittedTag, Note("Dune", none))

      assertTrue(
        cells == cats.data.Chain("title" -> "Dune"),
        row.map(_.headers.value.toList) == List("title", "tag").some,
        row.map(_.values.toList) == List("Dune", "").some
      )
    ,
    test("every row of a file agrees with the header on its columns"):
      val header = CsvHeaderRenderer.render(csv.blankTag).toList
      val rows = List(Note("Dune", 42.some), Note("Emma", none))
        .flatMap(CsvKeyedRowEncoder.encode(csv.blankTag, _))

      assertTrue(rows.length == 2, rows.forall(_.headers.value.toList == header))
    ,
    test("a row holds at least one cell, which an empty schema has none of"):
      assertTrue(CsvKeyedRowEncoder.encode(RNil, ()).isEmpty, CsvRowEncoder.encode(TNil, ()).isEmpty)
    ,
    test("the library owns the text, including the quoting Otter never sees"):
      val header = NonEmptyList.fromListUnsafe(CsvHeaderRenderer.render(csv.book).toList)
      val row = CsvKeyedRowEncoder.encode(csv.book, book)

      val text = Stream
        .emits(header :: row.toList.map(_.values))
        .through(lowlevel.toRowStrings[Pure]())
        .compile
        .string

      val parsed = Stream
        .emit(text)
        .through(lowlevel.rows[Fallible, String]())
        .through(lowlevel.headers[Fallible, String])
        .compile
        .toList
        .map(_.map(CsvKeyedRowDecoder.decode(csv.book, _)))

      assertTrue(
        text.startsWith("title,pages,read\n"),
        text.contains("\"\""),
        parsed == Right(List(book.valid))
      )
  )
