package io.taig.otter

sealed trait Schema[F[_], B]:
  final def optional: Schema[F, Option[B]] = ??? // Schema.Optional(this)

object Schema:
  sealed trait Required[F[_], B] extends Schema[F, B]

  object Required:
    final case class Root[F[_], B](a: F[B]) extends Schema.Required[F, B]

//   final case class Optional[+F[+_], S[+_], A <: F[S[Schema[F, S, ?, ?]]], B](self: Schema[F, S, A, B])
//       extends Schema[F, S, A, Option[B]]

//   final case class Root[+F[+_], S[+_], A <: F[S[Schema[F, S, ?, ?]]], B](fa: Data[F, S, A, B])
//       extends Schema[F, S, A, B]
