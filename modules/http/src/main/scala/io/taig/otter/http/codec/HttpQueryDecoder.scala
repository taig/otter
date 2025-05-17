package io.taig.otter.http.codec

import io.taig.otter.http.Query
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.http.Http.Query.Nullable
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Violation
import io.taig.otter.http.Query.Style
import io.taig.otter.unescape
import cats.data.Chain

final class HttpQueryDecoder(explode: Boolean, style: Query.Style)
    extends Decoder.Remainding[Http.Query, Chain[Option[String]]]:
  override def decodeRemainding[A](
      schema: Http.Query[A],
      values: Chain[Option[String]]
  ): Validated[Violations, (Chain[Option[String]], A)] = schema match
    case schema: Http.Query.Value[A] =>
      val (remainders, value) = values.collectFirstWithRemainders { case Some(value) => value }
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen(HttpQueryValueParser.decode(schema, _).tupleLeft(remainders))
    case schema: Http.Query.Array[A] =>
      values
        .traverse(_.toValid(Violations.rootNec(Violation.required)))
        .map: values =>
          (explode, style) match
            case (true, _)                       => values
            case (_, Query.Style.Form)           => decode(values, character = ",")
            case (_, Query.Style.PipeDelimited)  => decode(values, character = "|")
            case (_, Query.Style.SpaceDelimited) => decode(values, character = " ")
        .andThen(HttpQueryArrayDecoder.decode(schema, _))
        .tupleLeft(Chain.empty)
    case Http.Query.Nullable(self) =>
      NullableDecoder
        .Remainding(decoder = this, empty = _.exists(_.isEmpty))
        .decodeRemainding(schema = self, values)
        .map: (remainders, value) =>
          remainders.collectFirstWithRemainders { case None => () }.as(value)

  def decode(values: Chain[String], character: String): Chain[String] =
    values
      .flatMap(value => Chain.fromIterableOnce(value.split(character)))
      .map(unescape(_, character))
