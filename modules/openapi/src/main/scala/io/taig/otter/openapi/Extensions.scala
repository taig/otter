package io.taig.otter.openapi

import cats.data.Chain
import io.taig.otter.Data

opaque type Extensions = Chain[(String, Data)]

object Extensions:
  extension (self: Extensions) def toChain: Chain[(String, Data)] = self

  val Empty: Extensions = Chain.empty

  def apply(values: (String, Data)*): Extensions = Chain.fromSeq(values)
