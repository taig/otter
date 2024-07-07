package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.http as Base
import io.taig.otter.http.Plain.*
import org.typelevel.ci.CIString
import io.taig.otter.Decoder
import io.taig.otter.ValueStringDecoder
import cats.syntax.all.*

object HeaderDecoder:
  def apply[A](header: Header.Reader[A], headers: List[Http4sHeader.Raw]): Decoder.Result[Option[String], A] =
    withRemainders(header, headers).map { case (_, a) => a }

  def withRemainders[A](
      header: Header.Reader[A],
      headers: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] = header match
    case Base.Header.Root(name, schema)        => root(name, schema, headers)
    case Base.Header.Transform(self, f, _)     => transform(self, f, headers)
    case Base.Header.Reader.Root(name, schema) => root(name, schema, headers)
    case Base.Header.Reader.Transform(self, f) => transform(self, f, headers)

  def root[A](
      name: CIString,
      schema: Value.Reader[A],
      headers: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] =
    val (value, remainders) = headers.findWithRemainders { case Http4sHeader.Raw(`name`, value) => value }
    ValueStringDecoder(schema, value).tupleLeft(remainders)

  def transform[A, B](
      self: Header.Reader[A],
      f: A => B,
      headers: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], B)] = withRemainders(self, headers).map(_.map(f))
