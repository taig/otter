package io.taig.otter

type Parent[+F[+_]] = F[Schema[F, ?, ?]]

object Parent:
  type Reader[+F[+_]] = F[Schema.Reader[F, ?, ?]]

  type Writer[+F[+_]] = F[Schema.Writer[F, ?, ?]]

type Knot[+F[+_]] = [a] =>> F[Schema[F, ?, a]]

object Knot:
  type Reader[+F[+_]] = [a] =>> F[Schema.Reader[F, ?, a]]

  type Writer[+F[+_]] = [a] =>> F[Schema.Writer[F, ?, a]]

sealed trait Schema[+F[+_], +A <: Parent[F], B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  final def optional: Schema[F, A, Option[B]] = Schema.Optional(this)

object Schema:
  sealed trait Required[+F[+_], +A <: Parent[F], B] extends Schema[F, A, B]

  object Required:
    sealed trait Reader[+F[+_], +A <: Parent.Reader[F], +B] extends Schema.Reader[F, A, B]

    sealed trait Writer[+F[+_], +A <: Parent.Writer[F], -B] extends Schema.Writer[F, A, B]

    object Writer:
      final case class Root[F[+_], +A <: Parent.Writer[F], B](data: Data[Knot.Writer[F], A, B])
          extends Schema.Required.Writer[F, A, B]

    final case class Root[F[+_], +A <: Parent[F], B](
        data: Data[Knot[F], A, B]
    ) extends Schema.Required[F, A, B]

  sealed trait Reader[+F[+_], +A <: Parent.Reader[F], +B] extends Product, Serializable

  sealed trait Writer[+F[+_], +A <: Parent.Writer[F], -B] extends Product, Serializable

  object Writer:
    final case class Optional[F[+_], +A <: Parent.Writer[F], B](self: Schema.Writer[F, A, B])
        extends Schema.Writer[F, A, Option[B]]

  final case class Optional[F[+_], A <: Parent[F], B](self: Schema[F, A, B]) extends Schema[F, A, Option[B]]

  // final case class Validate[F[_], D[f[_], a, b] <: Data[f, a, b], A, B, C, E](
  //     self: Schema[F, D, A],
  //     validation: Validation[A, B, C, E],
  //     f: E => A
  // ) extends Schema[F, D, E]
