package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.Csv
import io.taig.otter.Keys
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object CsvEncoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvEncoderTest")(
    test("Csv.Primitive"):
      assertTrue(
        CsvCellEncoder.encode(string, "foobar") == "foobar",
        CsvCellEncoder.encode(int, 42) == "42",
        CsvCellEncoder.encode(boolean, true) == "true",
        CsvCellEncoder.encode(double, 1.5) == "1.5",
        CsvCellEncoder.encode(long, 42L) == "42"
      )
    ,
    test("Csv.Primitive: uuid"):
      val id = java.util.UUID.fromString("1c1a5f8e-6e33-4e34-8d2e-3f8b2f0e1a2b")
      assertTrue(CsvCellEncoder.encode(uuid, id) == id.toString)
    ,
    test("Csv.Primitive: a number has no cell of its own to be written in"):
      assertTrue(
        CsvCellEncoder.encode(int, 42) == CsvCellEncoder.encode(string, "42"),
        CsvCellEncoder.encode(boolean, false) == CsvCellEncoder.encode(string, "false")
      )
    ,
    test("Csv.Constant"):
      assertTrue(CsvCellEncoder.encode(constant(string, "foobar"), ()) == "foobar")
    ,
    test("Csv.Coerce"):
      assertTrue(CsvCellEncoder.encode(coerce(boolean), true) == "true")
    ,
    test("Csv.Enumeration"):
      assertTrue(CsvCellEncoder.encode(csv.genre, Genre.History) == "history")
    ,
    test("Csv.Optional"):
      assertTrue(
        CsvCellEncoder.encode(int.optional, 42.some) == "42",
        CsvCellEncoder.encode(int.optional, none) == ""
      )
    ,
    test("Csv.Record"):
      assertTrue(
        CsvRecordEncoder.encode(csv.book, Book("Dune", 412, true)) ==
          Chain("title" -> "Dune", "pages" -> "412", "read" -> "true")
      )
    ,
    test("Csv.Record: RNil"):
      assertTrue(CsvRecordEncoder.encode(RNil, ()) == Chain.empty)
    ,
    test("Csv.Record: an absent column keeps its place and is left empty"):
      assertTrue(
        CsvRecordEncoder.encode(csv.blankTag, Note("Dune", 42.some)) == Chain("title" -> "Dune", "tag" -> "42"),
        CsvRecordEncoder.encode(csv.blankTag, Note("Dune", none)) == Chain("title" -> "Dune", "tag" -> "")
      )
    ,
    test("Csv.Record: an omitted column is dropped, which shortens the row"):
      assertTrue(
        CsvRecordEncoder.encode(csv.omittedTag, Note("Dune", 42.some)) == Chain("title" -> "Dune", "tag" -> "42"),
        CsvRecordEncoder.encode(csv.omittedTag, Note("Dune", none)) == Chain("title" -> "Dune")
      )
    ,
    test("Csv.Record: a defaulted column always writes"):
      val schema = field("title", string) :* field("pages", int).optional(0)
      assertTrue(
        CsvRecordEncoder.encode(schema, ("Dune", 412)) == Chain("title" -> "Dune", "pages" -> "412"),
        CsvRecordEncoder.encode(schema, ("Dune", 0)) == Chain("title" -> "Dune", "pages" -> "0")
      )
    ,
    test("Csv.Record: the attribute survives .optional"):
      val schema = field("title", string) :* field("tag", int).omitted.optional
      assertTrue(CsvRecordEncoder.encode(schema, ("Dune", none)) == Chain("title" -> "Dune"))
    ,
    test("Csv.Record: a globally set attribute is read"):
      val schema = field("title", string) :* field("tag", int).optional.attr(Keys.absence, Absence.Omit)
      assertTrue(CsvRecordEncoder.encode(schema, ("Dune", none)) == Chain("title" -> "Dune"))
    ,
    test("Csv.Record: the csv namespace wins over the global one"):
      val schema = field("title", string) :* field("tag", int).optional
        .attr(Keys.absence, Absence.Omit)
        .attr(Csv.Namespace, Keys.absence, Absence.Empty)
      assertTrue(CsvRecordEncoder.encode(schema, ("Dune", none)) == Chain("title" -> "Dune", "tag" -> ""))
    ,
    test("Csv.Tuple"):
      assertTrue(CsvTupleEncoder.encode(csv.positional, Book("Dune", 412, true)) == Vector("Dune", "412", "true"))
    ,
    test("Csv.Tuple: TNil"):
      assertTrue(CsvTupleEncoder.encode(TNil, ()) == Vector.empty)
    ,
    test("Csv.Tuple: an absent cell keeps its place, so the cells after it stay where they are"):
      val schema = TNil :* string :* int.optional :* boolean
      assertTrue(
        CsvTupleEncoder.encode(schema, ("Dune", 412.some, true)) == Vector("Dune", "412", "true"),
        CsvTupleEncoder.encode(schema, ("Dune", none, true)) == Vector("Dune", "", "true")
      )
    ,
    test("a write only schema still writes"):
      assertTrue(CsvCellEncoder.encode(csv.title, Book("Dune", 412, true)) == "Dune")
  )
