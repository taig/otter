package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import io.taig.otter.http as Base
import io.taig.otter.http.Plain.*
import org.typelevel.ci.CIString
import io.taig.otter.Decoder
import io.taig.otter.ValueRequiredStringDecoder
import io.taig.otter.ValueStringDecoder

object HeaderDecoder:
  def apply[A](header: Header.Reader[A], headers: List[Http4sHeader.Raw]): Decoder.Result[Any, A] = ???

  def withRemainders[A](
      header: Header.Reader[A],
      headers: List[Http4sHeader.Raw]
  ): Decoder.Result[Any, (List[Http4sHeader.Raw], A)] = header match
    case Base.Header.Root(name, schema)        => root(name, schema, headers)
    case Base.Header.Transform(self, f, _)     => ???
    case Base.Header.Reader.Root(name, schema) => root(name, schema, headers)
    case Base.Header.Reader.Transform(self, f) => ???

  def root[A](
      name: CIString,
      schema: Value.Reader[A],
      headers: List[Http4sHeader.Raw]
  ): Decoder.Result[Any, (List[Http4sHeader.Raw], A)] =
    val (value, remainders) = headers.findWithRemainders { case Http4sHeader.Raw(`name`, value) => value }

    ValueStringDecoder(schema, value)
    ???
