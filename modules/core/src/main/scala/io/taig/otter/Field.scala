package io.taig.otter

import cats.syntax.all.*
import cats.Functor

sealed trait Field[F[+_], +A, B] extends Field.Reader[F, A, B], Field.Writer[F, A, B]:
  def key: F[Value.Required[F, ?, ?]]
  def value: F[Schema[F, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, B]

object Field:
  sealed trait Reader[F[+_], +A, +B]:
    def key: F[Value.Required.Reader[F, ?, ?]]
    def value: F[Schema.Reader[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, B]

  object Reader:
    final case class Root[F[+_], A, B <: F[Schema.Reader[F, ?, C]], C](key: F[Value.Required.Reader[F, ?, A]], value: B)
        extends Field.Reader[F, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, C] =
        copy(key = fK(key).map(_.translate(fK)), value = fK(value).map(_.translate(fK)))

  sealed trait Writer[F[+_], +A, -B]:
    def key: F[Value.Required.Writer[F, ?, ?]]
    def value: F[Schema.Writer[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, B]

  object Writer:
    final case class Root[F[+_], A, B <: F[Schema.Writer[F, ?, C]], C](key: F[Value.Required.Writer[F, ?, A]], value: B)
        extends Field.Writer[F, B, C]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, C] =
        copy(key = fK(key).map(_.translate(fK)), value = fK(value).map(_.translate(fK)))

  final case class Root[F[+_], A, B <: F[Schema[F, ?, C]], C](key: F[Value.Required[F, ?, A]], value: B)
      extends Field[F, B, C]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, C] =
      copy(key = fK(key).map(_.translate(fK)), value = fK(value).map(_.translate(fK)))
