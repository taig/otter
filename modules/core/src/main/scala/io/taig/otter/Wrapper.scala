package io.taig.otter

sealed trait Isomorphic[F[_], O[f[_], a] <: Optional[f, a], S[a, b] <: Schema[a, b], A]
    extends Writer[F, O, S, A],
      Reader[F, O, S, A]

object Isomorphic:
  final case class Root[F[_], O[f[_], a] <: Optional[f, a], S[a, b] <: Schema[a, b], A](
      fa: F[O[S[Isomorphic[F, Optional, Schema, ?], *], A]]
  ) extends Isomorphic[F, O, S, A]

sealed trait Writer[F[_], O[f[_], a] <: Optional[f, a], S[a, b] <: Schema[a, b], -A]

object Writer:
  final case class Root[F[_], O[f[_], a] <: Optional[f, a], S[a, b] <: Schema[a, b], A](
      fa: F[O[S[Writer[F, Optional, Schema, ?], *], A]]
  ) extends Writer[F, O, S, A]

sealed trait Reader[F[_], O[f[_], a] <: Optional[f, a], S[a, b] <: Schema[a, b], +A]
