package io.taig.otter.http.header

import cats.data.NonEmptyList
import cats.parse.Parser
import io.taig.otter.http.Parsers
import cats.implicits.*
import cats.Order
import io.taig.otter.http.Dsl.*

opaque type Accept = NonEmptyList[Weighted[MediaRange]]

object Accept:
  extension (self: Accept)
    inline def toNel: NonEmptyList[Weighted[MediaRange]] = self

    def toSortedList: List[MediaRange] = toNel
      // TODO the result should actually include a blocklist for q=0
      .filter(_.weight =!= BigDecimal(0).some)
      .sorted(sortOrdering)
      .map(_.self)

  def apply(values: NonEmptyList[Weighted[MediaRange]]): Accept = values

  def parse(value: String): Either[Parser.Error, Accept] = Parsers.accept.parseAll(value)

  val codec: Primitive.Required[Accept] = parser(name = "accept")(parse(_).toOption)(_.show)

  private val sortOrdering: Ordering[Weighted[MediaRange]] = 
    given Order[MediaRange.Type] =
      case (MediaRange.Type.Any, MediaRange.Type.Any) => 0
      case (MediaRange.Type.Secondary(_, _), MediaRange.Type.Secondary(_, _)) => 0
      case (MediaRange.Type.Primary(_), MediaRange.Type.Primary(_)) => 0
      case (MediaRange.Type.Any, _) => -1
      case (MediaRange.Type.Secondary(_, _), _) => 1
      case (MediaRange.Type.Primary(_), MediaRange.Type.Any) => 1
      case (MediaRange.Type.Primary(_), MediaRange.Type.Secondary(_, _)) => -1

    given Order[List[Parameter]] = Order.by(_.length)

    given Order[MediaRange] = Order.by(mediaRange => (mediaRange.tpe, mediaRange.parameters))

    summon[Order[Weighted[MediaRange]]].toOrdering.reverse
