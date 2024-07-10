package io.taig.otter

import cats.syntax.all.*
import cats.Functor

sealed trait Field[+F[+_], -A, +B, C] extends Field.Reader[F, A, B, C], Field.Writer[F, A, B, C]:
  override def name(value: String): Field[F, A, B, C]

  def nulls(value: Field.Null): Field[F, A, B, C]

  override def schema: F[Schema[F, A, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, A, ?, C]

object Field:
  sealed trait Reader[+F[+_], -A, +B, +C]:
    def name: String
    def name(value: String): Field.Reader[F, A, B, C]

    def schema: F[Schema.Reader[F, A, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, A, ?, C]

  object Reader:
    final case class Root[F[+_], A, +B <: F[Schema.Reader[F, A, ?, C]], C](name: String, schema: B)
        extends Field.Reader[F, A, B, C]:
      override def name(value: String): Field.Reader[F, A, B, C] = copy(name = name)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], -A, +B, -C]:
    def name: String
    def name(value: String): Field.Writer[F, A, B, C]

    def nulls: Field.Null
    def nulls(value: Field.Null): Field.Writer[F, A, B, C]

    def schema: F[Schema.Writer[F, A, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, A, ?, C]

  object Writer:
    final case class Root[F[+_], A, +B <: F[Schema.Writer[F, A, ?, C]], C](name: String, nulls: Field.Null, schema: B)
        extends Field.Writer[F, A, B, C]:
      override def name(value: String): Field.Writer[F, A, B, C] = copy(name = name)
      override def nulls(value: Null): Field.Writer[F, A, B, C] = copy(nulls = nulls)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, A, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Root[F[+_], A, +B <: F[Schema[F, A, ?, C]], C](name: String, nulls: Field.Null, schema: B)
      extends Field[F, A, B, C]:
    override def name(value: String): Field[F, A, B, C] = copy(name = name)
    override def nulls(value: Null): Field[F, A, B, C] = copy(nulls = nulls)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, A, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))

  enum Null:
    case Hide
    case Show
    case Inherit

  object Null:
    val Default: Field.Null = Inherit
