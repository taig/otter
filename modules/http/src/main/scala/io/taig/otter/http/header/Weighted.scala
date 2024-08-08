package io.taig.otter.http.header

import cats.Show
import cats.syntax.all.*
import io.taig.otter.http.Printers

final case class Weighted[A](self: A, weight: Option[BigDecimal])

object Weighted:
  given [A: Show]: Show[Weighted[A]] = Printers(_)
