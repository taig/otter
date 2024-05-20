package io.taig.otter

sealed trait Schema[+F[_], B] extends Schema.Reader[F, B], Schema.Writer[F, B]:
  final def optional: Schema[F, Option[B]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[_], B] extends Schema[F, B]

  object Required:
    sealed trait Reader[+F[_], +B] extends Schema.Reader[F, B]

    sealed trait Writer[+F[_], -B] extends Schema.Writer[F, B]

    object Writer:
      final case class Root[F[+_], B](data: Data[F, B]) extends Schema.Required.Writer[F, B]

    final case class Root[F[+_], B](data: Data[F, B]) extends Schema.Required[F, B]

  sealed trait Reader[+F[_], +B] extends Product, Serializable

  sealed trait Writer[+F[_], -B] extends Product, Serializable

  object Writer:
    final case class Optional[F[_], B](self: Schema.Writer[F, B]) extends Schema.Writer[F, Option[B]]

  final case class Optional[F[_], B](self: Schema[F, B]) extends Schema[F, Option[B]]
