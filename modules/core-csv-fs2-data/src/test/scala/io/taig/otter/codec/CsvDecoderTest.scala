package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.*
import io.taig.validation.Violation
import zio.Scope
import zio.test.*

object CsvDecoderTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvDecoderTest")(
    test("Csv.Primitive"):
      assertTrue(
        CsvCellDecoder.decode(string, "foobar") == "foobar".valid,
        CsvCellDecoder.decode(int, "42") == 42.valid,
        CsvCellDecoder.decode(boolean, "true") == true.valid,
        CsvCellDecoder.decode(double, "1.5") == 1.5.valid,
        CsvCellDecoder.decode(long, "42") == 42L.valid
      )
    ,
    test("Csv.Primitive: uuid"):
      val id = java.util.UUID.fromString("1c1a5f8e-6e33-4e34-8d2e-3f8b2f0e1a2b")
      assertTrue(
        CsvCellDecoder.decode(uuid, id.toString) == id.valid,
        CsvCellDecoder.decode(uuid, "not-a-uuid").isInvalid
      )
    ,
    test("Csv.Primitive: the cell that failed is what the violation reports"):
      val result = CsvCellDecoder.decode(int, "4 2")
      val expected = Violations(
        Violation(constraint = Constraint.Generic.Type(name = "int"), actual = "4 2", hint = none)
      ).invalid

      assertTrue(result == expected)
    ,
    test("Csv.Primitive: text cannot be the wrong shape"):
      assertTrue(CsvCellDecoder.decode(string, "") == "".valid)
    ,
    test("Csv.Coerce: accepts the spellings a spreadsheet writes"):
      assertTrue(
        CsvCellDecoder.decode(coerce(boolean), "yes") == true.valid,
        CsvCellDecoder.decode(coerce(boolean), "NO") == false.valid,
        CsvCellDecoder.decode(coerce(boolean), " true ") == true.valid,
        CsvCellDecoder.decode(coerce(int), " 42 ") == 42.valid,
        CsvCellDecoder.decode(coerce(int), "+42") == 42.valid,
        CsvCellDecoder.decode(coerce(string), "  padded  ") == "padded".valid
      )
    ,
    test("Csv.Coerce: what it does not recognise still fails"):
      assertTrue(CsvCellDecoder.decode(coerce(int), "nope").isInvalid)
    ,
    test("Csv.Primitive: strict text keeps its spaces"):
      assertTrue(CsvCellDecoder.decode(string, "  padded  ") == "  padded  ".valid)
    ,
    test("Csv.Constant"):
      assertTrue(
        CsvCellDecoder.decode(constant(string, "foobar"), "foobar") == ().valid,
        CsvCellDecoder.decode(constant(string, "foobar"), "barfoo").isInvalid
      )
    ,
    test("Csv.Enumeration"):
      assertTrue(
        CsvCellDecoder.decode(csv.genre, "history") == Genre.History.valid,
        CsvCellDecoder.decode(csv.genre, "nope").isInvalid
      )
    ,
    test("Csv.Optional"):
      assertTrue(
        CsvCellDecoder.decode(int.optional, "42") == 42.some.valid,
        CsvCellDecoder.decode(int.optional, "") == none.valid
      )
    ,
    test("Csv.Record"):
      val row = Chain("title" -> "Dune", "pages" -> "412", "read" -> "true")
      assertTrue(CsvRecordDecoder.decode(csv.book, row) == Book("Dune", 412, true).valid)
    ,
    test("Csv.Record: a missing column fails"):
      assertTrue(CsvRecordDecoder.decode(csv.book, Chain("title" -> "Dune", "pages" -> "412")).isInvalid)
    ,
    test("Csv.Record: leniency takes a missing column and an empty cell alike"):
      assertTrue(
        CsvRecordDecoder.decode(csv.blankTag, Chain("title" -> "Dune", "tag" -> "")) == Note("Dune", none).valid,
        CsvRecordDecoder.decode(csv.blankTag, Chain("title" -> "Dune")) == Note("Dune", none).valid,
        CsvRecordDecoder.decode(csv.blankTag, Chain("title" -> "Dune", "tag" -> "42")) == Note("Dune", 42.some).valid
      )
    ,
    test("Csv.Record: a strict blank column wants its column, empty"):
      val schema = field("tag", int).optional.strict.toRecord
      assertTrue(
        CsvRecordDecoder.decode(schema, Chain("tag" -> "")) == none.valid,
        CsvRecordDecoder.decode(schema, Chain("tag" -> "42")) == 42.some.valid,
        CsvRecordDecoder.decode(schema, Chain.empty).isInvalid
      )
    ,
    test("Csv.Record: a strict omitted column wants no column at all"):
      val schema = field("tag", int).optional.omitted.strict.toRecord
      assertTrue(
        CsvRecordDecoder.decode(schema, Chain.empty) == none.valid,
        CsvRecordDecoder.decode(schema, Chain("tag" -> "42")) == 42.some.valid,
        CsvRecordDecoder.decode(schema, Chain("tag" -> "")).isInvalid
      )
    ,
    test("Csv.Record: only a strict column tells two layers of absence apart"):
      val lenient = field("tag", int.optional).optional.toRecord
      assertTrue(
        CsvRecordDecoder.decode(csv.nestedTag, Chain.empty) == none.valid,
        CsvRecordDecoder.decode(csv.nestedTag, Chain("tag" -> "")) == none.some.valid,
        CsvRecordDecoder.decode(csv.nestedTag, Chain("tag" -> "42")) == 42.some.some.valid,
        CsvRecordDecoder.decode(lenient, Chain("tag" -> "")) == none.valid
      )
    ,
    test("Csv.Record: a defaulted column falls back when its cell is empty"):
      val schema = field("pages", int).optional(0).toRecord
      assertTrue(
        CsvRecordDecoder.decode(schema, Chain("pages" -> "412")) == 412.valid,
        CsvRecordDecoder.decode(schema, Chain("pages" -> "")) == 0.valid,
        CsvRecordDecoder.decode(schema, Chain.empty) == 0.valid
      )
    ,
    test("Csv.Tuple"):
      assertTrue(CsvTupleDecoder.decode(csv.positional, Vector("Dune", "412", "true")) == Book("Dune", 412, true).valid)
    ,
    test("Csv.Tuple: wrong arity"):
      assertTrue(
        CsvTupleDecoder.decode(csv.positional, Vector("Dune", "412")).isInvalid,
        CsvTupleDecoder.decode(csv.positional, Vector("Dune", "412", "true", "extra")).isInvalid
      )
    ,
    test("violations carry the path to the failure"):
      val steps = CsvRecordDecoder.decode(csv.book, Chain("title" -> "Dune", "pages" -> "nope", "read" -> "true")) match
        case Validated.Invalid(violations) => paths(violations)
        case Validated.Valid(_)            => Nil

      assertTrue(steps == List(List(Step.Field("pages"))))
    ,
    test("a read only schema still reads"):
      assertTrue(CsvCellDecoder.decode(csv.isbn, "978") == Isbn("978").valid)
  )

  private def paths(violations: Violations): List[List[Step]] = violations match
    case Violations.Root(values, _) =>
      if values.isEmpty then List(Nil)
      else values.toList.flatMap((step, nested) => paths(nested).map(step :: _))
    case Violations.Namespace(values) =>
      values.toSortedMap.toList.flatMap((step, nested) => paths(nested).map(step :: _))
