package io.taig.otter.http

import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.all.*
import fs2.Fallible
import fs2.Pure
import fs2.Stream
import fs2.data.csv.CsvRow
import fs2.data.csv.EscapeMode
import fs2.data.csv.Row
import fs2.data.csv.lowlevel
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.CsvHeaderRenderer
import io.taig.otter.codec.CsvKeyedRowDecoder
import io.taig.otter.codec.CsvKeyedRowEncoder
import io.taig.otter.codec.CsvRowDecoder
import io.taig.otter.codec.CsvRowEncoder
import io.taig.otter.http.codec.Http4sPayload
import io.taig.validation.Violation
import scodec.bits.ByteVector

import scala.compiletime.asMatchable

/** Buffered CSV bodies, using fs2-data for the CSV wire syntax and Otter for each row's schema. */
object Http4sFs2Data:
  val Payload: Http4sPayload[CsvDocument] = Http4sPayload[CsvDocument]([W, R] =>
    (payload: Any) =>
      payload.asMatchable match
        case csv: CsvDocument[W, R] @unchecked => Some(csv)
        case _                                 => None
  )(new Http4sPayload.Codec[CsvDocument]:
    override def decode[R](payload: CsvDocument[Nothing, R], bytes: ByteVector): Validated[Violations, R] =
      payload match
        case CsvDocument.Rows(schema)         => Http4sFs2Data.decodeRows(schema.value, bytes, indexed = true)
        case row: CsvDocument.Row[Nothing, R] =>
          Http4sFs2Data
            .decodeRows(row, bytes, indexed = false)
            .andThen:
              case Vector(value) => value.valid
              case values        => Http4sFs2Data.violation("Exactly one CSV data row", values.length.toString).invalid

    override def encode[W](payload: CsvDocument[W, Any], value: W): Either[String, ByteVector] =
      payload match
        case CsvDocument.Rows(schema)     => Http4sFs2Data.encodeRows(schema.value, value)
        case row: CsvDocument.Row[W, Any] => Http4sFs2Data.encodeRows(row, Vector(value)))

  private def violation(expected: String, actual: String): Violations =
    Violations(Violation(constraint = Constraint.Generic.Type(expected), actual = actual.asData, hint = none))

  private def parse(bytes: ByteVector): Validated[Violations, Vector[Row]] =
    bytes.decodeUtf8
      .leftMap(error => Http4sFs2Data.violation("UTF-8", error.getMessage))
      .toValidated
      .andThen: text =>
        Stream
          .emit(text)
          .through(lowlevel.rows[Fallible, String]())
          .compile
          .toVector
          .leftMap(error => Http4sFs2Data.violation("CSV", error.getMessage))
          .toValidated

  private def decodeRows[R](
      schema: CsvDocument.Row[Nothing, R],
      bytes: ByteVector,
      indexed: Boolean
  ): Validated[Violations, Vector[R]] =
    Http4sFs2Data
      .parse(bytes)
      .andThen: rows =>
        schema match
          case CsvDocument.Record(reference) =>
            rows.headOption
              .toValid(Http4sFs2Data.violation("CSV header", "empty document"))
              .andThen: header =>
                rows.tail.zipWithIndex.traverse: (row, index) =>
                  val decoded = CsvRow(row.values, header.values)
                    .leftMap(error => Http4sFs2Data.violation("CSV row matching its header", error.getMessage))
                    .toValidated
                    .andThen(CsvKeyedRowDecoder.decode(reference.value, _))
                  if indexed then decoded.leftMap(index /: _) else decoded
          case CsvDocument.Tuple(reference) =>
            rows.zipWithIndex.traverse: (row, index) =>
              val decoded = CsvRowDecoder.decode(reference.value, row)
              if indexed then decoded.leftMap(index /: _) else decoded

  private def encodeRows[W](schema: CsvDocument.Row[W, Any], values: Vector[W]): Either[String, ByteVector] =
    val rows: Either[String, Vector[NonEmptyList[String]]] = schema match
      case CsvDocument.Record(reference) =>
        for
          header <- NonEmptyList
            .fromList(CsvHeaderRenderer.render(reference.value).toList)
            .toRight("A CSV record must have at least one column")
          rows <- values.traverse(value =>
            CsvKeyedRowEncoder
              .encode(reference.value, value)
              .toRight("A CSV record must have at least one column")
              .map(_.values)
          )
        yield header +: rows
      case CsvDocument.Tuple(reference) =>
        if reference.value.self.self.schemas.isEmpty then Left("A CSV tuple must have at least one column")
        else
          values.traverse(value =>
            CsvRowEncoder
              .encode(reference.value, value)
              .toRight("A CSV tuple must have at least one column")
              .map(_.values)
          )

    rows.flatMap: rows =>
      val text = Stream
        .emits(rows)
        .flatMap: row =>
          // Auto escaping leaves singleton empty cells blank and treats a trailing CR as part of the line ending.
          val escape =
            if row == NonEmptyList.one("") || row.exists(_.contains('\r')) then EscapeMode.Always
            else EscapeMode.Auto
          Stream.emit(row).through(lowlevel.toRowStrings[Pure](escape = escape))
        .compile
        .string
      ByteVector.encodeUtf8(text).leftMap(_.getMessage)
