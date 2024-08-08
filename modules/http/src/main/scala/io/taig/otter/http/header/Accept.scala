package io.taig.otter.http.header

import cats.data.NonEmptyList
import cats.parse.Parser
import io.taig.otter.http.Parsers
import cats.syntax.all.*

opaque type Accept = NonEmptyList[Weighted[MediaRange]]

object Accept:
  extension (self: Accept)
    inline def toNel: NonEmptyList[Weighted[MediaRange]] = self

    def toSortedList: List[MediaRange] = toNel
      .filter(_.weight =!= BigDecimal(0).some)
      .sortBy(_.weight.getOrElse(BigDecimal(1)))(Ordering[BigDecimal].reverse)
      .map(_.self)

  def apply(values: NonEmptyList[Weighted[MediaRange]]): Accept = values

  def parse(value: String): Either[Parser.Error, Accept] = Parsers.accept.parseAll(value)
