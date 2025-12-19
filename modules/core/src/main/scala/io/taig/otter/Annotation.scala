package io.taig.otter

import cats.Applicative
import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self

final case class Annotation[+A](metadata: Metadata, self: A):
  def modify(f: Metadata => Metadata): Annotation[A] = copy(metadata = f(metadata))

  def map[B](f: A => B): Annotation[B] = copy(self = f(self))

object Annotation:
  def apply[A](self: A): Annotation[A] = Annotation(metadata = Metadata.Empty, self)

  given applicative: Applicative[Annotation]:
    override def map[A, B](fa: Annotation[A])(f: A => B): Annotation[B] = fa.copy(self = f(fa.self))

    override def ap[A, B](ff: Annotation[A => B])(fa: Annotation[A]): Annotation[B] =
      Annotation(metadata = fa.metadata ++ ff.metadata, self = ff.self(fa.self))

    override def pure[A](a: A): Annotation[A] = Annotation(self = a)

  given annotated: [A] => Annotated[Annotation[A]]:
    override def get(self: Annotation[A]): Metadata = self.metadata

    override def modify(self: Annotation[A], metadata: Metadata => Metadata): Annotation[A] =
      self.modify(metadata)

  given contravariant: [F[_]: Contravariant] => Contravariant[[a] =>> Annotation[F[a]]]:
    override def contramap[A, B](fa: Annotation[F[A]])(f: B => A): Annotation[F[B]] =
      fa.map(_.contramap(f))

  given functor: [F[_]: Functor] => Functor[[a] =>> Annotation[F[a]]]:
    override def map[A, B](fa: Annotation[F[A]])(f: A => B): Annotation[F[B]] =
      fa.map(_.map(f))

  given invariant: [F[_]: Invariant] => Invariant[[a] =>> Annotation[F[a]]]:
    override def imap[A, B](fa: Annotation[F[A]])(f: A => B)(g: B => A): Annotation[F[B]] =
      fa.map(_.imap(f)(g))

  given invariantK: [F[_[_]]: InvariantK, G[_]] => (F: F[G]) => F[[a] =>> Annotation[G[a]]] =
    F.imapK[[a] =>> Annotation[G[a]]]([a] => (ga: G[a]) => Annotation(ga))([a] =>
      (annotation: Annotation[G[a]]) => annotation.self
    )

  given invariantK2: [F[_[+_[a] <: f[a], _], f[_]]: InvariantK2, G[+_[a] <: H[a], _], H[_]]
    => (
        F: F[G, H]
  ) => F[[s[a] <: H[a], a] =>> Annotation[G[s, a]], H] =
    F.imapK[[s[a] <: H[a], a] =>> Annotation[G[s, a]]]([s[a] <: H[a], a] => (gsa: G[s, a]) => Annotation(gsa))(
      [s[a] <: H[a], a] => (annotation: Annotation[G[s, a]]) => annotation.self
    )

  given invariantK3: [
      F[_[+_[a] <: f[a], _], _[+_[a] <: f[a], a] <: f[a], f[_]]: InvariantK3,
      G[+_[a] <: I[a], _],
      H[+_[a] <: I[a], a] <: I[a],
      I[_]
  ]
    => (
        F: F[G, H, I]
  ) => F[[s[a] <: I[a], a] =>> Annotation[G[s, a]], H, I] =
    F.imapK[[s[a] <: I[a], a] =>> Annotation[G[s, a]]]([s[a] <: I[a], a] => (gsa: G[s, a]) => Annotation(gsa))(
      [s[a] <: I[a], a] => (annotation: Annotation[G[s, a]]) => annotation.self
    )

  given invariantK9: [
      F[
          _[+s[a] <: bound[a], a] <: read[s, a] & write[s, a],
          read[+_[a] <: boundRead[a], _],
          write[+_[a] <: boundWrite[a], _],
          _[+s[a] <: bound[a], a] <: bound[a],
          _[+s[a] <: boundRead[a], a] <: boundRead[a],
          _[+s[a] <: boundWrite[a], a] <: boundWrite[a],
          bound[a] <: boundRead[a] & boundWrite[a],
          boundRead[_],
          boundWrite[_]
      ]: InvariantK9,
      Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
      SelfRead[+_[a] <: BoundRead[a], _],
      SelfWrite[+_[a] <: BoundWrite[a], _],
      Schema[+_[a] <: Bound[a], a] <: Bound[a],
      SchemaRead[+_[a] <: BoundRead[a], a] <: BoundRead[a],
      SchemaWrite[+_[a] <: BoundWrite[a], a] <: BoundWrite[a],
      Bound[a] <: BoundRead[a] & BoundWrite[a],
      BoundRead[_],
      BoundWrite[_]
  ]
    => (
        F: F[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite]
  )
    => F[
      [s[a] <: Bound[a], a] =>> Annotation[Self[s, a]],
      [s[a] <: BoundRead[a], a] =>> Annotation[SelfRead[s, a]],
      [s[a] <: BoundWrite[a], a] =>> Annotation[SelfWrite[s, a]],
      Schema,
      SchemaRead,
      SchemaWrite,
      Bound,
      BoundRead,
      BoundWrite
    ] =
    InvariantK9[F].imapK[Self, SelfRead, SelfWrite, Schema, SchemaRead, SchemaWrite, Bound, BoundRead, BoundWrite](F)[
      [s[a] <: Bound[a], a] =>> Annotation[Self[s, a]],
      [s[a] <: BoundRead[a], a] =>> Annotation[SelfRead[s, a]],
      [s[a] <: BoundWrite[a], a] =>> Annotation[SelfWrite[s, a]]
    ](
      [s[a] <: Bound[a], a] => (self: Self[s, a]) => Annotation(self),
      [s[a] <: Bound[a], a] => (annotation: Annotation[Self[s, a]]) => annotation.self
    )(
      [s[a] <: BoundRead[a], a] => (selfRead: SelfRead[s, a]) => Annotation(selfRead),
      [s[a] <: BoundRead[a], a] => (annotation: Annotation[SelfRead[s, a]]) => annotation.self
    )(
      [s[a] <: BoundWrite[a], a] => (selfWrite: SelfWrite[s, a]) => Annotation(selfWrite),
      [s[a] <: BoundWrite[a], a] => (annotation: Annotation[SelfWrite[s, a]]) => annotation.self
    )
