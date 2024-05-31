package io.taig.otter

import io.taig.otter.validation.Validation

sealed trait Isomorphic[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B]
    extends Writer[F, O, S, A, B],
      Reader[F, O, S, A, B]:
  def extract: Extract[O[S[A, *], *], ?]
  def ivalidate[C](validation: Validation[B, ?, ?, C])(f: C => B): Isomorphic[F, O, S, A, C] =
    Isomorphic.Modify(this, validation, f)

object Isomorphic:
  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B](
      fa: O[S[A, *], B]
  ) extends Isomorphic[F, O, S, A, B]:
    override def extract: Extract[O[S[A, *], *], B] = Extract(fa)

  final case class Modify[F[+_], O[+_[_], _], S[+_, _], A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B, C](
      self: Isomorphic[F, O, S, A, B],
      validation: Validation[B, ?, ?, C],
      f: C => B
  ) extends Isomorphic[F, O, S, A, C]:
    override def extract: Extract[O[S[A, *], *], ?] = self.extract

  final case class Transform[
      F[+_],
      O[+_[_], _],
      P[+_[_], _],
      S[+_, _],
      A <: F[Isomorphic[F, Optional, Schema, ?, ?]],
      B,
      C
  ](self: Isomorphic[F, O, S, A, B], f: O[S[A, *], B] => P[S[A, *], C])
      extends Isomorphic[F, P, S, A, C]:
    override def extract: Extract[P[S[A, *], *], ?] =
      self.extract
      ??? // Extract(f.apply(self.extract.fa))

  extension [F[+_], O[+_[_], _], S[+_, _], A <: F[Isomorphic[F, Optional, Schema, ?, ?]], B](
      self: Isomorphic[F, O, S, A, B]
  ) def transform[P[+_[_], _], C](f: O[S[A, *], B] => P[S[A, *], C]): Isomorphic[F, P, S, A, C] = Transform(self, f)

sealed trait Reader[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Reader[F, Optional, Schema, ?, ?]], +B]:
  def validate[C](validation: Validation[B, ?, ?, C]): Reader[F, O, S, A, C] = Reader.Modify(this, validation)

object Reader:
  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: F[Reader[F, Optional, Schema, ?, ?]], B](fa: O[S[A, *], B])
      extends Reader[F, O, S, A, B]

  final case class Modify[F[+_], O[+_[_], _], S[+_, _], A <: F[Reader[F, Optional, Schema, ?, ?]], B, C](
      self: Reader[F, O, S, A, B],
      validation: Validation[B, ?, ?, C]
  ) extends Reader[F, O, S, A, C]

sealed trait Writer[+F[+_], +O[+_[_], _], +S[+_, _], +A <: F[Writer[F, Optional, Schema, ?, ?]], -B]:
  final def contramap[C](f: C => B): Writer[F, O, S, A, C] = Writer.Modify(this, f)

object Writer:
  final case class Root[F[+_], O[+_[_], _], S[+_, _], A <: F[Writer[F, Optional, Schema, ?, ?]], B](fa: O[S[A, *], B])
      extends Writer[F, O, S, A, B]

  final case class Modify[F[+_], O[+_[_], _], S[+_, _], A <: F[Writer[F, Optional, Schema, ?, ?]], B, C](
      self: Writer[F, O, S, A, B],
      f: C => B
  ) extends Writer[F, O, S, A, C]
