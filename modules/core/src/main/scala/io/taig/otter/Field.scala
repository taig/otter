package io.taig.otter

import cats.syntax.all.*
import cats.Functor

sealed trait Field[-A, +B, C] extends Field.Reader[A, B, C], Field.Writer[A, B, C]:
  override def name(value: String): Field[A, B, C]

  def nulls(value: Field.Null): Field[A, B, C]

  override def schema: Schema[A, ?, ?]

object Field:
  sealed trait Reader[-A, +B, +C]:
    def name: String
    def name(value: String): Field.Reader[A, B, C]

    def schema: Schema.Reader[A, ?, ?]

  object Reader:
    final case class Root[F[+_], A, +B <: Schema.Reader[A, ?, C], C](name: String, schema: B)
        extends Field.Reader[A, B, C]:
      override def name(value: String): Field.Reader[A, B, C] = copy(name = name)

  sealed trait Writer[-A, +B, -C]:
    def name: String
    def name(value: String): Field.Writer[A, B, C]

    def nulls: Field.Null
    def nulls(value: Field.Null): Field.Writer[A, B, C]

    def schema: Schema.Writer[A, ?, ?]

  object Writer:
    final case class Root[F[+_], A, +B <: Schema.Writer[A, ?, C], C](name: String, nulls: Field.Null, schema: B)
        extends Field.Writer[A, B, C]:
      override def name(value: String): Field.Writer[A, B, C] = copy(name = name)
      override def nulls(value: Null): Field.Writer[A, B, C] = copy(nulls = nulls)

  final case class Root[F[+_], A, +B <: Schema[A, ?, C], C](name: String, nulls: Field.Null, schema: B)
      extends Field[A, B, C]:
    override def name(value: String): Field[A, B, C] = copy(name = name)
    override def nulls(value: Null): Field[A, B, C] = copy(nulls = nulls)

  enum Null:
    case Hide
    case Show
    case Inherit

  object Null:
    val Default: Field.Null = Inherit
