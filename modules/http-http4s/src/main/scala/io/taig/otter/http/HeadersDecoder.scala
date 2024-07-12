package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import io.taig.otter.http.*
import io.taig.otter.*
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated

object HeadersDecoder:
  def apply[A](headers: Headers.Reader[A], values: Http4sHeaders): Decoder.Result[Option[String], A] =
    withRemainders(headers, values.headers).map { case (_, a) => a }

  def withRemainders[A](
      headers: Headers.Reader[A],
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] = headers match
    case Headers.Empty                       => (values, ()).valid
    case Headers.One(header)                 => one(header, values)
    case Headers.Reader.One(header)          => one(header, values)
    case Headers.Combine(left, right)        => combine(left, right, values)
    case Headers.Reader.Combine(left, right) => combine(left, right, values)

  def one[A](
      header: Header.Reader[A],
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] = HeaderDecoder(header, values)

  def combine[A, B](
      left: Headers.Reader[A],
      right: Headers.Reader[B],
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], (A, B))] = withRemainders(left, values) match
    case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      withRemainders(right, values).fold(violations.combine, _ => violations).invalid
