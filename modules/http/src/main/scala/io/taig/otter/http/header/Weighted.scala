package io.taig.otter.http.header

import cats.Show
import cats.syntax.all.*
import io.taig.otter.http.Printers
import cats.Order

final case class Weighted[A](self: A, weight: Option[BigDecimal])

object Weighted:
  given [A: Order]: Order[Weighted[A]] =
    Order.by(weighted => (weighted.weight.getOrElse(BigDecimal(1)), weighted.self))

  given [A: Show]: Show[Weighted[A]] = Printers(_)
