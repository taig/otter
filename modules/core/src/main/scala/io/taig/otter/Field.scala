package io.taig.otter

import cats.syntax.all.*
import cats.Functor
import cats.Id as Identity
import cats.Show
import io.taig.otter.Decoder.Result
import cats.kernel.Eq

sealed trait Field[+F[+_], +A, B] extends Field.Reader[F, A, B], Field.Writer[F, A, B]:
  def schema: F[Schema[F, ?, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, B]

object Field:
  sealed trait Reader[+F[+_], +A, +B]:
    def matches(value: String): Decoder.Result[String, Boolean]
    def name: String
    def schema: F[Schema.Reader[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, B]

  object Reader:
    final case class Root[F[+_], A: Show: Eq, B <: F[Schema.Reader[F, ?, C]], C](
        identifier: A,
        key: Value.Required[Identity, ?, A],
        schema: B
    ) extends Field.Reader[F, B, C]:
      override def matches(value: String): Result[String, Boolean] =
        StringDecoder(key, value).map(_ === identifier)
      override def name: String = StringEncoder(key, identifier)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Reader[G, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  sealed trait Writer[+F[+_], +A, -B]:
    def name: String
    def schema: F[Schema.Writer[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, B]

  object Writer:
    final case class Root[F[+_], A, B <: F[Schema.Writer[F, ?, C]], C](
        identifier: A,
        key: Value.Required.Writer[Identity, ?, A],
        schema: B
    ) extends Field.Writer[F, B, C]:
      override def name: String = StringEncoder(key, identifier)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field.Writer[G, ?, C] =
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Root[F[+_], A: Show: Eq, B <: F[Schema[F, ?, C]], C](
      identifier: A,
      key: Value.Required[Identity, ?, A],
      schema: B
  ) extends Field[F, B, C]:
    override def matches(value: String): Result[String, Boolean] =
      StringDecoder(key, value).map(_ === identifier)
    override def name: String = StringEncoder(key, identifier)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Field[G, ?, C] =
      copy(schema = fK(schema).map(_.translate(fK)))
