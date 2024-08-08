package io.taig.otter.http.header

import cats.Show
import cats.syntax.all.*

final case class Weighted[A](self: A, weight: Option[Float])

object Weighted:
  given [A: Show]: Show[Weighted[A]] =
    case Weighted(self, None)         => self.show
    case Weighted(self, Some(weight)) => show"$self; q=$weight"
