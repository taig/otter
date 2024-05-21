package io.taig.otter

sealed trait Schema[+F[_], A] extends Schema.Reader[F, A], Schema.Writer[F, A]:
  final def optional: Schema[F, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[_], A] extends Schema[F, A]

  object Required:
    sealed trait Reader[+F[_], +A] extends Schema.Reader[F, A]

    sealed trait Writer[+F[_], -A] extends Schema.Writer[F, A]

    object Writer:
      final case class Root[F[_], A](data: F[Data[F, A]]) extends Schema.Required.Writer[F, A]

    final case class Root[F[_], A](data: F[Data[F, A]]) extends Schema.Required[F, A]

  sealed trait Reader[+F[_], +A] extends Product, Serializable

  sealed trait Writer[+F[_], -A] extends Product, Serializable

  object Writer:
    final case class Optional[F[_], A](self: Schema.Writer[F, A]) extends Schema.Writer[F, Option[A]]

  final case class Optional[+F[_], A](self: Schema[F, A]) extends Schema[F, Option[A]]
