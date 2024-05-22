package io.taig.otter

sealed trait Schema[F[_], A]:
  final def optional: Schema[F, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Required[F[_], A] extends Schema[F, A]

  object Required:
    final case class Root[F[_], A](fa: F[A]) extends Schema.Required[F, A]

  final case class Optional[F[_], A](self: Schema[F, A]) extends Schema[F, Option[A]]

  final case class Root[F[_], A](fa: F[A]) extends Schema[F, A]
