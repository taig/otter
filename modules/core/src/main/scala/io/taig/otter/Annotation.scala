package io.taig.otter

import cats.Applicative
import cats.Apply
import cats.Contravariant
import cats.ContravariantSemigroupal
import cats.Functor
import cats.Invariant
import cats.InvariantSemigroupal
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
    extension (self: Annotation[A])
      override def metadata: Metadata = self.metadata

      override def modify(f: Metadata => Metadata): Annotation[A] = self.modify(f)

  given apply: [F[_]: Apply] => Apply[[a] =>> Annotation[F[a]]]:
    override def ap[A, B](ff: Annotation[F[A => B]])(fa: Annotation[F[A]]): Annotation[F[B]] =
      Annotation(metadata = Metadata.Empty, self = ff.self.ap(fa.self))

    override def map[A, B](fa: Annotation[F[A]])(f: A => B): Annotation[F[B]] =
      fa.map(_.map(f))

  given contravariant: [F[_]: Contravariant] => Contravariant[[a] =>> Annotation[F[a]]]:
    override def contramap[A, B](fa: Annotation[F[A]])(f: B => A): Annotation[F[B]] =
      fa.map(_.contramap(f))

  given contravariantSemigroupal
      : [F[_]: ContravariantSemigroupal] => ContravariantSemigroupal[[a] =>> Annotation[F[a]]]:
    override def contramap[A, B](fa: Annotation[F[A]])(f: B => A): Annotation[F[B]] =
      fa.map(_.contramap(f))

    override def product[A, B](fa: Annotation[F[A]], fb: Annotation[F[B]]): Annotation[F[(A, B)]] =
      Annotation(metadata = Metadata.Empty, self = fa.self.product(fb.self))

  given functor: [F[_]: Functor] => Functor[[a] =>> Annotation[F[a]]]:
    override def map[A, B](fa: Annotation[F[A]])(f: A => B): Annotation[F[B]] =
      fa.map(_.map(f))

  given invariant: [F[_]: Invariant] => Invariant[[a] =>> Annotation[F[a]]]:
    override def imap[A, B](fa: Annotation[F[A]])(f: A => B)(g: B => A): Annotation[F[B]] =
      fa.map(_.imap(f)(g))

  given invariantSemigroupal: [F[_]: InvariantSemigroupal] => InvariantSemigroupal[[a] =>> Annotation[F[a]]]:
    override def imap[A, B](fa: Annotation[F[A]])(f: A => B)(g: B => A): Annotation[F[B]] =
      fa.map(_.imap(f)(g))

    override def product[A, B](fa: Annotation[F[A]], fb: Annotation[F[B]]): Annotation[F[(A, B)]] =
      Annotation(metadata = Metadata.Empty, self = fa.self.product(fb.self))

  given invariantK: [F[_[_]]: InvariantK, G[_]] => (F: F[G]) => F[[a] =>> Annotation[G[a]]] =
    F.imapK[[a] =>> Annotation[G[a]]]([a] => (ga: G[a]) => Annotation(ga))([a] =>
      (annotation: Annotation[G[a]]) => annotation.self
    )

  given invariantK2: [F[_[_], _[_]], G[_], H[_]] => (InvariantK[[f[_]] =>> F[f, H]]) => (F: F[G, H])
    => F[[a] =>> Annotation[G[a]], H] =
    F.imapK[[a] =>> Annotation[G[a]]]([a] => (ga: G[a]) => Annotation(ga))([a] =>
      (annotation: Annotation[G[a]]) => annotation.self
    )
