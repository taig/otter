package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.http.*
import org.typelevel.ci.CIString
import io.taig.otter.*
import io.taig.otter.ValueStringDecoder
import cats.syntax.all.*

object HeaderDecoder:
  def apply[A](
      header: Header.Reader[A],
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] = header match
    case Header.Root(_, name, schema)        => root(name, schema, values)
    case Header.Transform(self, f, _)        => transform(self, f, values)
    case Header.Reader.Root(_, name, schema) => root(name, schema, values)
    case Header.Reader.Transform(self, f)    => transform(self, f, values)

  def root[A](
      name: CIString,
      schema: Value.Reader.Via[String, A],
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], A)] =
    val (value, remainders) = values.findWithRemainders { case Http4sHeader.Raw(`name`, value) => value }
    ValueStringDecoder(schema, value).tupleLeft(remainders)

  def transform[A, B](
      self: Header.Reader[A],
      f: A => B,
      values: List[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (List[Http4sHeader.Raw], B)] = HeaderDecoder(self, values).map(_.map(f))
