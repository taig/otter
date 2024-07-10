package io.taig.otter

import cats.Functor
import cats.syntax.all.*

sealed trait Branch[+F[+_], -A, +B, C] extends Branch.Reader[F, A, B, C], Branch.Writer[F, A, B, C]:
  override def schema: F[Schema[F, A, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch[G, A, ?, C]

object Branch:
  sealed trait Reader[+F[+_], -A, +B, +C]:
    def name: String
    def schema: F[Schema.Reader[F, A, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Reader[G, A, ?, C]

  object Reader:
    final case class Root[F[+_], A, +B <: F[Schema.Reader[F, A, ?, C]], C](name: String, schema: B)
        extends Branch.Reader[F, A, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Reader[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], -A, +B, -C]:
    def name: String
    def schema: F[Schema.Writer[F, A, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Writer[G, A, ?, C]

  object Writer:
    final case class Root[F[+_], A, +B <: F[Schema.Writer[F, A, ?, C]], C](name: String, schema: B)
        extends Branch.Writer[F, A, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Writer[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Root[F[+_], A, +B <: F[Schema[F, A, ?, C]], C](name: String, schema: B) extends Branch[F, A, B, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch[G, A, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))
