package io.taig.otter

sealed trait Schema[+F[+_], +A <: F[Schema[F, ?, ?]], B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  final def optional: Schema[F, A, Option[B]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[+_], +A <: F[Schema[F, ?, ?]], B] extends Schema[F, A, B]

  object Required:
    sealed trait Reader[+F[+_], +A <: F[Schema.Reader[F, ?, ?]], +B] extends Schema.Reader[F, A, B]

    sealed trait Writer[+F[+_], +A <: F[Schema.Writer[F, ?, ?]], -B] extends Schema.Writer[F, A, B]

    object Writer:
      final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, ?]], B](
          data: Data[[a] =>> F[Schema.Writer[F, ?, a]], A, B]
      ) extends Schema.Required.Writer[F, A, B]

    final case class Root[F[+_], +A <: F[Schema[F, ?, ?]], B](data: Data[[a] =>> F[Schema.Writer[F, ?, a]], A, B])
        extends Schema.Required[F, A, B]

  sealed trait Reader[+F[+_], +A <: F[Schema.Reader[F, ?, ?]], +B] extends Product, Serializable

  sealed trait Writer[+F[+_], +A <: F[Schema.Writer[F, ?, ?]], -B] extends Product, Serializable

  object Writer:
    final case class Optional[F[+_], +A <: F[Schema.Writer[F, ?, ?]], B](self: Schema.Writer[F, A, B])
        extends Schema.Writer[F, A, Option[B]]

  final case class Optional[F[+_], A <: F[Schema[F, ?, ?]], B](self: Schema[F, A, B]) extends Schema[F, A, Option[B]]

  // final case class Validate[F[_], D[f[_], a, b] <: Data[f, a, b], A, B, C, E](
  //     self: Schema[F, D, A],
  //     validation: Validation[A, B, C, E],
  //     f: E => A
  // ) extends Schema[F, D, E]
