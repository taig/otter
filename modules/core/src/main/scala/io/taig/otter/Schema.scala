package io.taig.otter

import io.taig.otter.validation.Validation

sealed trait Schema[+F[_], +D[f[_], a, b] <: Data[f, a, b], A] extends Schema.Reader[F, D, A], Schema.Writer[F, D, A]:
  final def optional: Schema[F, D, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[_], +D[f[_], a, b] <: Data[f, a, b], A] extends Schema[F, D, A]

  object Required:
    sealed trait Reader[+F[_], +D[f[_], a, b] <: Data[f, a, b], +A] extends Schema.Reader[F, D, A]

    sealed trait Writer[+F[_], +D[f[_], a, b] <: Data[f, a, b], -A] extends Schema.Writer[F, D, A]

    object Writer:
      final case class Root[F[_], D[f[_], a, b] <: Data[f, a, b], A](data: D[[a] =>> F[Schema.Writer[F, ?, a]], Any, A])
          extends Schema.Required.Writer[F, D, A]

    final case class Root[F[_], D[f[_], a, b] <: Data[f, a, b], A](data: D[[a] =>> F[Schema[F, ?, a]], Any, A])
        extends Schema.Required[F, D, A]

  sealed trait Reader[+F[_], +D[f[_], a, b] <: Data[f, a, b], +A] extends Product, Serializable

  sealed trait Writer[+F[_], +D[f[_], a, b] <: Data[f, a, b], -A] extends Product, Serializable

  object Writer:
    final case class Optional[F[_], D[f[_], a, b] <: Data[f, a, b], A](self: Schema.Writer[F, D, A])
        extends Schema.Writer[F, D, Option[A]]

  final case class Optional[F[_], D[f[_], a, b] <: Data[f, a, b], A](self: Schema[F, D, A])
      extends Schema[F, D, Option[A]]

  final case class Validate[F[_], D[f[_], a, b] <: Data[f, a, b], A, B, C, E](
      self: Schema[F, D, A],
      validation: Validation[A, B, C, E],
      f: E => A
  ) extends Schema[F, D, E]
