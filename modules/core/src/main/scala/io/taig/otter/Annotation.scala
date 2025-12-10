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

  given applicative: Applicative[Annotation] with
    override def map[A, B](fa: Annotation[A])(f: A => B): Annotation[B] = fa.copy(self = f(fa.self))

    override def ap[A, B](ff: Annotation[A => B])(fa: Annotation[A]): Annotation[B] =
      Annotation(metadata = fa.metadata ++ ff.metadata, self = ff.self(fa.self))

    override def pure[A](a: A): Annotation[A] = Annotation(self = a)

  given annotated[A]: Annotated[Annotation[A]] with
    override def get(self: Annotation[A]): Metadata = self.metadata

    override def modify(self: Annotation[A], metadata: Metadata => Metadata): Annotation[A] =
      self.modify(metadata)

  given contravariant[F[_]: Contravariant]: Contravariant[[a] =>> Annotation[F[a]]] with
    override def contramap[A, B](fa: Annotation[F[A]])(f: B => A): Annotation[F[B]] =
      fa.map(_.contramap(f))

  given functor[F[_]: Functor]: Functor[[a] =>> Annotation[F[a]]] with
    override def map[A, B](fa: Annotation[F[A]])(f: A => B): Annotation[F[B]] =
      fa.map(_.map(f))

  given invariant[F[_]: Invariant]: Invariant[[a] =>> Annotation[F[a]]] with
    override def imap[A, B](fa: Annotation[F[A]])(f: A => B)(g: B => A): Annotation[F[B]] =
      fa.map(_.imap(f)(g))

  given [F[_[_]], G[_]](using F: F[G])(using InvariantK[F]): F[[a] =>> Annotation[G[a]]] =
    F.imapK[[a] =>> Annotation[G[a]]]([a] => (ga: G[a]) => Annotation(ga))([a] =>
      (annotation: Annotation[G[a]]) => annotation.self
    )

  given [F[_[+_[a] <: f[a], _], f[_]], G[+_[a] <: H[a], _], H[_]](using
      F: F[G, H]
  )(using InvariantK2[F]): F[[s[a] <: H[a], a] =>> Annotation[G[s, a]], H] =
    F.imapK[[s[a] <: H[a], a] =>> Annotation[G[s, a]]]([s[a] <: H[a], a] => (gsa: G[s, a]) => Annotation(gsa))(
      [s[a] <: H[a], a] => (annotation: Annotation[G[s, a]]) => annotation.self
    )

  given [F[_[+_[_], _], _[+_[_], _], _[_]], H[+_[_], _], G[+_[_], _], I[_]](using
      F: F[H, G, I]
  )(using InvariantK3[F]): F[[s[_], a] =>> Annotation[H[s, a]], G, I] =
    F.imapK[[s[_], a] =>> Annotation[H[s, a]]]([s[_], a] => (hsa: H[s, a]) => Annotation(hsa))([s[_], a] =>
      (annotation: Annotation[H[s, a]]) => annotation.self
    )
