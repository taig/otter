package io.taig.otter

sealed trait Isomorphic[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B]
    extends Writer[F, O, S, A, B],
      Reader[F, O, S, A, B]

object Isomorphic:
  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B](
      fa: O[S[A, *], B]
  ) extends Isomorphic[F, O, S, A, B]

sealed trait Writer[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Writer[F, Optional, Schema, ?, ?]], -B]:
  final def contramap[C](f: C => B): Writer[F, O, S, A, C] = Writer.Modify(this, f)

object Writer:
  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: F[Writer[F, Optional, Schema, ?, ?]], B](fa: O[S[A, *], B])
      extends Writer[F, O, S, A, B]

  final case class Modify[F[+_], O[+_[_], _], S[+_, _], A <: F[Writer[F, Optional, Schema, ?, ?]], B, C](
      self: Writer[F, O, S, A, B],
      f: C => B
  ) extends Writer[F, O, S, A, C]

sealed trait Reader[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Reader[F, Optional, Schema, ?, ?]], +B]

object Reader:
  final case class Root[+F[+_], O[+_[_], _], S[+_, _], +A <: F[Reader[F, Optional, Schema, ?, ?]], B](fa: O[S[A, *], B])
      extends Reader[F, O, S, A, B]
