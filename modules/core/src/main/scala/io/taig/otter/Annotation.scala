package io.taig.otter

import io.taig.otter as Plain
import cats.syntax.all.*
import cats.Functor

final case class Annotation[+S, +M[+_]](self: S, metadata: M[Annotation[S, M]])

object Annotation:
  extension [S, M[+_]: Functor](self: Annotation[S, M])
    def map[T](f: S => T): Annotation[T, M] = transform(f, _.map(f))

    def transform[T](f: S => T, g: Annotation[S, M] => Annotation[T, M]): Annotation[T, M] =
      Annotation(f(self.self), self.metadata.map(g))
