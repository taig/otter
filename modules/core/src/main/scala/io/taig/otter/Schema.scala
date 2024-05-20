package io.taig.otter

sealed trait Schema[+F[_], +D[f[+_], a] <: Data[f, a], A] extends Schema.Reader[F, D, A], Schema.Writer[F, D, A]:
  final def optional: Schema[F, D, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[_], +D[f[+_], a] <: Data[f, a], A] extends Schema[F, D, A]

  object Required:
    sealed trait Reader[+F[_], +D[f[+_], a] <: Data[f, a], +A] extends Schema.Reader[F, D, A]

    sealed trait Writer[+F[_], +D[f[+_], a] <: Data[f, a], -A] extends Schema.Writer[F, D, A]

    object Writer:
      final case class Root[F[+_], D[f[+_], a] <: Data[f, a], A](data: D[F, A]) extends Schema.Required.Writer[F, D, A]

    final case class Root[F[+_], D[f[+_], a] <: Data[f, a], A](data: D[F, A]) extends Schema.Required[F, D, A]

  sealed trait Reader[+F[_], +D[f[+_], a] <: Data[f, a], +A] extends Product, Serializable

  sealed trait Writer[+F[_], +D[f[+_], a] <: Data[f, a], -A] extends Product, Serializable

  object Writer:
    final case class Optional[F[_], D[f[+_], a] <: Data[f, a], A](self: Schema.Writer[F, D, A])
        extends Schema.Writer[F, D, Option[A]]

  final case class Optional[F[_], D[f[+_], a] <: Data[f, a], A](self: Schema[F, D, A]) extends Schema[F, D, Option[A]]
