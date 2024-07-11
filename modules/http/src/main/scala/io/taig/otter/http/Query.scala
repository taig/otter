package io.taig.otter.http

import io.taig.otter.Value
import cats.Functor
import cats.syntax.all.*

sealed trait Query[+F[+_], +A]:
  def schema: F[Value[F, String, ?, ?]]
  def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Query[G, A]

object Query:
  final case class Root[F[+_], A](name: String, schema: F[Value[F, String, ?, A]]) extends Query[F, A]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Query[G, A] =
      copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[F[+_], A, B](self: Query[F, A], f: A => B) extends Query[F, B]:
    export self.schema
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Query[G, B] =
      copy(self = self.translate(fK))
