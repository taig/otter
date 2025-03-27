package io.taig.otter

import cats.Order
import cats.Show
import cats.data.Chain
import cats.parse.Parser

opaque type XPath = Chain[Step]

object XPath:
  val Root: XPath = Chain.empty

  extension (self: XPath)
    inline def toChain: Chain[Step] = self
    def /(step: Step): XPath = toChain :+ step
    def /(field: String): XPath = /(Step.Field(field))
    def /(index: Int): XPath = /(Step.Index(index))

  def apply(steps: Chain[Step]): XPath = steps

  def parse(value: String): Either[Parser.Error, XPath] = ??? // Parsers.xpath.parseAll(value)

  given (using order: Order[Chain[Step]]): Order[XPath] = order

  given Show[XPath] = ??? // Printers(_)
