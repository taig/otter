package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Headers

object HeadersDataEncoder extends Encoder[Headers, Headers.Data]:
  override def encode[A](headers: Headers[A], a: A): Headers.Data =
    encode(headers = headers.value, a)

  def encode[A](headers: Headers.Value[A], a: A): Headers.Data = headers match
    case Headers.Value.Empty              => Chain.empty
    case Headers.Value.Optional(self)     => a.fold(Chain.empty)(encode(headers = self, _))
    case Headers.Value.Modify(self, _, g) => encode(headers = self, g(a))
    case Headers.Value.Root(header)       => Chain.fromOption(HeaderDataEncoder.encode(header, a))
    case Headers.Value.Zip(left, right)   => encode(headers = left, a._1) ++ encode(headers = right, a._2)
