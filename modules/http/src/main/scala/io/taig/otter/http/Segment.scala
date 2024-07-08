package io.taig.otter.http

import io.taig.otter.Value
import cats.Functor
import cats.syntax.all.*

sealed trait Segment[+F[+_], +A]:
  def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Segment[G, A]

object Segment:
  final case class Static(name: String) extends Segment[Nothing, Unit]:
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Segment[Nothing, Unit] = this

  final case class Parameter[F[+_], A](name: String, schema: F[Value.Required.Reader[F, ?, A]]) extends Segment[F, A]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Segment[G, A] =
      copy(schema = fK(schema).map(_.translate(fK)))
