package io.taig.otter

import cats.data.Chain
import cats.parse.Parser
import cats.Order
import cats.Show

opaque type XPath = Chain[Step]

object XPath:
  val Empty: XPath = Chain.empty

  extension (self: XPath)
    inline def toChain: Chain[Step] = self
    def /(step: Step): XPath = toChain :+ step

  def apply(steps: Chain[Step]): XPath = steps

  def parse(value: String): Either[Parser.Error, XPath] = Parsers.xpath.parseAll(value)

  given (using order: Order[Chain[Step]]): Order[XPath] = order

  given Show[XPath] = Printers(_)
