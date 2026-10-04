package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Csv
import io.taig.otter.component.CsvComponent.*
import zio.Scope
import zio.test.*

object CsvAbsenceTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvAbsenceTest")(
    suite("optional contracts")(
      List[(String, Csv.Field[Option[Int]], Boolean, Boolean, Chain[(String, String)])](
        ("optional", field("x", int).optional, true, false, Chain.empty),
        ("blank", field("x", int).blank, false, true, Chain("x" -> "")),
        ("optionalOrBlank", field("x", int).optionalOrBlank, true, true, Chain.empty),
        ("blankOrMissing", field("x", int).blankOrMissing, true, true, Chain("x" -> ""))
      ).map { (name, field, missing, blank, written) =>
        test(name):
          val schema = field.toRecord
          assertTrue(
            CsvRecordDecoder.decode(schema, Fields.empty).isValid == missing,
            CsvRecordDecoder.decode(schema, Fields("x" -> "")).isValid == blank,
            CsvRecordDecoder.decode(schema, Fields("x" -> "3")) == Validated.valid(Some(3)),
            CsvRecordDecoder.decode(schema, Fields("x" -> "bad")).isInvalid,
            CsvRecordEncoder.encode(schema, None) == written,
            CsvRecordEncoder.encode(schema, Some(3)) == Chain("x" -> "3")
          )
      }
    ),
    test("defaults name their trigger and always write their supplied value"):
      val missing = field("x", int).defaulted(7).toRecord
      val blank = field("x", int).defaultedOnBlank(7).toRecord
      val either = field("x", int).defaultedOnMissingOrBlank(7).toRecord
      assertTrue(
        CsvRecordDecoder.decode(missing, Fields.empty) == Validated.valid(7),
        CsvRecordDecoder.decode(missing, Fields("x" -> "")).isInvalid,
        CsvRecordDecoder.decode(blank, Fields.empty).isInvalid,
        CsvRecordDecoder.decode(blank, Fields("x" -> "")) == Validated.valid(7),
        CsvRecordDecoder.decode(either, Fields.empty) == Validated.valid(7),
        CsvRecordDecoder.decode(either, Fields("x" -> "")) == Validated.valid(7),
        CsvRecordDecoder.decode(either, Fields("x" -> "bad")).isInvalid,
        CsvRecordEncoder.encode(either, 7) == Chain("x" -> "7")
      )
  )
