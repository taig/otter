package io.taig.otter.http.header

import cats.Order
import cats.Show
import cats.syntax.all.*

final case class Weighted[A](self: A, weight: Option[BigDecimal])

object Weighted:
  given order[A: Order]: Order[Weighted[A]] =
    Order.by(weighted => (weighted.weight.getOrElse(BigDecimal(1)), weighted.self))

  given [A: Show]: Show[Weighted[A]] =
    case Weighted(self, None)         => self.show
    case Weighted(self, Some(weight)) => show"$self; q=$weight"
