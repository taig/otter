package io.taig.otter

sealed trait Isomorphic[F[a], G[a] >: F[a], O[_[_], _], S[_, _], A <: Isomorphic.Any[G], B]
    extends Writer[F, G, O, S, A, B],
      Reader[F, G, O, S, A, B]

object Isomorphic:
  type Of[F[_], A] = Isomorphic[F, F, Optional, Schema, ?, A]

  type Any[F[_]] = Isomorphic[F, F, Optional, Schema, ?, ?]

  final case class Root[F[_], G[a] >: F[a], O[_[_], _], S[_, _], A <: Isomorphic.Any[G], B](fa: F[O[S[A, *], B]])
      extends Isomorphic[F, G, O, S, A, B]

sealed trait Writer[F[_], G[a] >: F[a], O[_[_], _], S[_, _], A <: Writer.Any[G], -B]

object Writer:
  type Any[F[_]] = Writer[F, F, Optional, Schema, ?, ?]

  final case class Root[F[_], G[a] >: F[a], O[_[_], _], S[_, _], A <: Writer.Any[G], B](fa: F[O[S[A, *], B]])
      extends Writer[F, G, O, S, A, B]

sealed trait Reader[F[_], G[a] >: F[a], O[_[_], _], S[_, _], A <: Reader.Any[G], +B]

object Reader:
  type Any[F[_]] = Reader[F, F, Optional, Schema, ?, ?]

  final case class Root[F[_], G[a] >: F[a], O[_[_], _], S[_, _], A <: Reader.Any[G], B](fa: F[O[S[A, *], B]])
      extends Reader[F, G, O, S, A, B]
