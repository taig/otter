package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import io.taig.otter.http.*
import io.taig.otter.*
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated
import cats.data.Chain

object HeadersDecoder:
  def apply[A](headers: Headers[A], values: Http4sHeaders): Decoder.Result[Option[String], A] =
    withRemainders(headers, Chain.fromSeq(values.headers)).map { case (_, a) => a }

  def withRemainders[A](
      headers: Headers[A],
      values: Chain[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (Chain[Http4sHeader.Raw], A)] = headers match
    case Headers.Empty       => (values, ()).valid
    case Headers.One(header) => HeaderDecoder(header, values)
    case Headers.Combine(left, right) =>
      withRemainders(left, values) match
        case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          withRemainders(right, values).fold(violations.combine, _ => violations).invalid
