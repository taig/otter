package io.taig.otter.http

import cats.data.Chain

object HeadersDataEncoder:
  def apply[A](headers: Headers[A], a: A): Headers.Data = headers match
    case Headers.Empty              => Chain.empty
    case Headers.Optional(self)     => a.fold(Chain.empty)(apply(headers = self, _))
    case Headers.Modify(self, _, g) => apply(headers = self, g(a))
    case Headers.Root(header)       => Chain.fromOption(HeaderDataEncoder(header, a))
    case Headers.Zip(left, right)   => apply(headers = left, a._1) ++ apply(headers = right, a._2)
