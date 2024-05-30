package io.taig.otter

sealed trait Isomorphic[F[_], O[_[_], _], S[_, _], +A <: Isomorphic.Any[F], B]
    extends Writer[F, O, S, A, B],
      Reader[F, O, S, A, B]

object Isomorphic:
  type Any[F[_]] = Isomorphic[F, Optional, Schema, ?, ?]

  final case class Root[F[_], O[_[_], _], S[_, _], A <: Isomorphic.Any[F], B](fa: F[O[S[A, *], B]])
      extends Isomorphic[F, O, S, A, B]

sealed trait Writer[F[_], O[_[_], _], S[_, _], +A <: WriterAny[F], -B]

type WriterAny[F[_]] = Writer[F, Optional, Schema, ?, ?]

object Writer:

  final case class Root[F[_], O[_[_], _], S[_, _], A <: WriterAny[F], B](fa: F[O[S[A, *], B]])
      extends Writer[F, O, S, A, B]

sealed trait Reader[F[_], O[_[_], _], S[_, _], +A <: Reader.Any[F], +B]

object Reader:
  type Any[F[_]] = Reader[F, Optional, Schema, ?, ?]

  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: Reader.Any[F], B](fa: F[O[S[A, *], B]])
      extends Reader[F, O, S, A, B]
