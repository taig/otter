package io.taig.otter.http.header

import cats.Order
import cats.data.Ior
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.Parser
import io.taig.otter.http.Parsers

opaque type Accept = NonEmptyList[Weighted[MediaRange]]

object Accept:
  type Result = Ior[NonEmptyList[MediaRange], NonEmptyList[MediaRange]]

  extension (self: Accept)
    inline def toNel: NonEmptyList[Weighted[MediaRange]] = self

    def toResult: Accept.Result =
      val blocklist = toNel.filter(_.weight === BigDecimal(0).some).map(_.self)

      blocklist.toNel match
        case None => Ior.right(toNel.sorted(resultOrder).map(_.self))
        case Some(blocklist) =>
          toNel
            .filter(_.weight =!= BigDecimal(0).some)
            .sorted(resultOrder.toOrdering)
            .map(_.self)
            .toNel
            .fold(Ior.left(blocklist))(Ior.both(blocklist, _))

  def apply(values: NonEmptyList[Weighted[MediaRange]]): Accept = values

  def parse(value: String): Either[Parser.Error, Accept] = Parsers.accept.parseAll(value)

  private val resultOrder: Order[Weighted[MediaRange]] =
    given Order[MediaRange.Type] =
      case (MediaRange.Type.Any, MediaRange.Type.Any)                         => 0
      case (MediaRange.Type.Secondary(_, _), MediaRange.Type.Secondary(_, _)) => 0
      case (MediaRange.Type.Primary(_), MediaRange.Type.Primary(_))           => 0
      case (MediaRange.Type.Any, _)                                           => -1
      case (MediaRange.Type.Secondary(_, _), _)                               => 1
      case (MediaRange.Type.Primary(_), MediaRange.Type.Any)                  => 1
      case (MediaRange.Type.Primary(_), MediaRange.Type.Secondary(_, _))      => -1

    given Order[Parameters] = Order.by(_.toList.length)

    given mediaRange: Order[MediaRange] = Order.by(mediaRange => (mediaRange.tpe, mediaRange.parameters))

    Order.reverse(Weighted.order(using mediaRange))
