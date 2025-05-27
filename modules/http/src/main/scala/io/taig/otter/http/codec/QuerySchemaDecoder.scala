package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.http.Query.Schema.Nullable
import io.taig.otter.unescape
import io.taig.otter.http.Query

final class QuerySchemaDecoder(explode: Boolean, style: Query.Style)
    extends Decoder.Remainding[Query.Schema, Chain[Option[String]]]:
  override def decodeRemainding[A](
      schema: Query.Schema[A],
      values: Chain[Option[String]]
  ): Validated[Violations, (Chain[Option[String]], A)] = schema match
    case schema: Query.Schema.Atom[A] =>
      val (remainders, value) = values.collectFirstWithRemainders { case Some(value) => value }
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen(QuerySchemaAtomParser.decode(schema, _).tupleLeft(remainders))
    case schema: Query.Schema.Array[A] =>
      values
        .traverse(_.toValid(Violations.rootNec(Violation.required)))
        .map: values =>
          (explode, style) match
            case (true, _)                       => values
            case (_, Query.Style.Form)           => decode(values, character = ",")
            case (_, Query.Style.PipeDelimited)  => decode(values, character = "|")
            case (_, Query.Style.SpaceDelimited) => decode(values, character = " ")
        .andThen(QuerySchemaArrayDecoder.decode(schema, _))
        .tupleLeft(Chain.empty)
    case Query.Schema.Nullable(self) =>
      NullableDecoder
        .Remainding(decoder = this, empty = _.exists(_.isEmpty))
        .decodeRemainding(schema = self.self, values)
        .map: (remainders, value) =>
          remainders.collectFirstWithRemainders { case None => () }.as(value)

  def decode(values: Chain[String], character: String): Chain[String] = values
    .flatMap(value => Chain.fromIterableOnce(value.split(character)))
    .map(unescape(_, character))
