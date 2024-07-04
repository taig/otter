package io.taig.otter

import cats.syntax.all.*
import cats.Functor

sealed trait Field[+F[+_], +A, B] extends Field.Reader[F, A, B], Field.Writer[F, A, B]:
  def name: String
  def schema: F[Schema[F, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, B]

object Field:
  sealed trait Reader[+F[+_], +A, +B]:
    def name: String
    def schema: F[Schema.Reader[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, B]

  object Reader:
    final case class Root[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](name: String, schema: A)
        extends Field.Reader[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], +A, -B]:
    def name: String
    def schema: F[Schema.Writer[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, B]

  object Writer:
    final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](name: String, schema: A)
        extends Field.Writer[F, A, B]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, B] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](name: String, schema: A) extends Field[F, A, B]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, B] =
      copy(schema = fK(schema).map(_.translate(fK)))
