package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Csv
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object CsvRoundTripTest extends ZIOSpecDefault:
  private def roundTrips[A](schema: Csv[A], value: A): TestResult =
    val encoded = CsvRecordEncoder.encode(schema, value)
    assertTrue(CsvRecordDecoder.decode(schema, encoded) == Validated.valid(value))

  private def roundTripsPositionally[A](schema: Csv.Tuple[A], value: A): TestResult =
    val encoded = CsvTupleEncoder.encode(schema, value)
    assertTrue(CsvTupleDecoder.decode(schema, encoded) == Validated.valid(value))

  private val book: Gen[Any, Book] = for
    title <- Gen.alphaNumericString
    pages <- Gen.int
    read <- Gen.boolean
  yield Book(title, pages, read)

  private val note: Gen[Any, Note] = for
    title <- Gen.alphaNumericString
    tag <- Gen.option(Gen.int)
  yield Note(title, tag)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvRoundTripTest")(
    test("a keyed row"):
      check(book)(roundTrips(csv.book, _))
    ,
    test("a positional row"):
      check(book)(roundTripsPositionally(csv.positional, _))
    ,
    test("an absent column left empty"):
      check(note)(roundTrips(csv.blankTag, _))
    ,
    test("an absent column dropped"):
      check(note)(roundTrips(csv.omittedTag, _))
    ,
    test("an enumeration"):
      check(Gen.fromIterable(Genre.values))(genre => roundTrips(field("genre", csv.genre).toRecord, genre))
    ,
    /** NaN is left out on purpose: it round trips through the text but is not equal to itself. */
    test("a number survives being written as text"):
      check(Gen.double(-1e9, 1e9), Gen.long)((d, l) =>
        roundTrips(field("value", double).toRecord, d) && roundTrips(field("value", long).toRecord, l)
      )
  )
