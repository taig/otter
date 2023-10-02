package io.taig.otter.openapi

import cats.data.Chain

opaque type Paths = Chain[(String, PathItem)]

object Paths:
  extension (self: Paths) def toChain: Chain[(String, PathItem)] = self

  val Empty: Paths = Chain.empty

  def fromChain(values: Chain[(String, PathItem)]): Paths = values
  def fromIterableOnce(values: IterableOnce[(String, PathItem)]): Paths = Chain.fromIterableOnce(values)
  def apply(values: (String, PathItem)*): Paths = Chain.fromSeq(values)
