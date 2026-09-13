package io.taig.otter.http.syntax

import io.taig.otter.Csv
import io.taig.otter.Reference
import io.taig.otter.http.Body
import io.taig.otter.http.CsvDocument
import io.taig.otter.http.MediaType

import scala.annotation.targetName

/** Whole CSV documents. Collections are buffered values, independent of streamed HTTP bodies. */
trait HttpCsvSyntax:
  @targetName("csvRecord")
  def csv[W, R](schema: => Csv.Record.Node[W, R]): Body.Schema[Body.Whole[CsvDocument], W, R] =
    Body.Schema(
      Body.Value.Whole(MediaType("text", "csv"), Reference.later(CsvDocument.Record(Reference.later(schema))))
    )

  @targetName("csvTuple")
  def csv[W, R](schema: => Csv.Tuple.Node[W, R]): Body.Schema[Body.Whole[CsvDocument], W, R] =
    Body.Schema(Body.Value.Whole(MediaType("text", "csv"), Reference.later(CsvDocument.Tuple(Reference.later(schema)))))

  @targetName("csvRecords")
  def csvRows[W, R](schema: => Csv.Record.Node[W, R]): Body.Schema[Body.Whole[CsvDocument], Vector[W], Vector[R]] =
    Body.Schema(
      Body.Value.Whole[CsvDocument, Vector[W], Vector[R]](
        MediaType("text", "csv"),
        Reference.later(CsvDocument.Rows(Reference.later(CsvDocument.Record(Reference.later(schema)))))
      )
    )

  @targetName("csvTuples")
  def csvRows[W, R](schema: => Csv.Tuple.Node[W, R]): Body.Schema[Body.Whole[CsvDocument], Vector[W], Vector[R]] =
    Body.Schema(
      Body.Value.Whole[CsvDocument, Vector[W], Vector[R]](
        MediaType("text", "csv"),
        Reference.later(CsvDocument.Rows(Reference.later(CsvDocument.Tuple(Reference.later(schema)))))
      )
    )

object HttpCsvSyntax extends HttpCsvSyntax
