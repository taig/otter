package io.taig.otter

import cats.Applicative
import cats.Invariant
import cats.syntax.all.*

final case class Annotation[+A](metadata: Metadata, self: A):
  def modifyMetadata(f: Metadata => Metadata): Annotation[A] = copy(metadata = f(metadata))

  def map[B](f: A => B): Annotation[B] = copy(self = f(self))

object Annotation:
  def apply[A](self: A): Annotation[A] = Annotation(metadata = Metadata.Empty, self)

  given applicative: Applicative[Annotation] with
    override def map[A, B](fa: Annotation[A])(f: A => B): Annotation[B] = fa.copy(self = f(fa.self))

    override def ap[A, B](ff: Annotation[A => B])(fa: Annotation[A]): Annotation[B] =
      Annotation(metadata = fa.metadata ++ ff.metadata, self = ff.self(fa.self))

    override def pure[A](x: A): Annotation[A] = Annotation(self = x)

  given annotated[A]: Annotated[Annotation[A]] with
    override def get(self: Annotation[A]): Metadata = self.metadata

    override def update(self: Annotation[A], metadata: Metadata => Metadata): Annotation[A] =
      self.modifyMetadata(metadata)

  given invariant[F[_]: Invariant]: Invariant[[a] =>> Annotation[F[a]]] with
    override def imap[A, B](fa: Annotation[F[A]])(f: A => B)(g: B => A): Annotation[F[B]] =
      fa.map(_.imap(f)(g))

  given operation1[F[_[_]], G[_]](using fg: F[G])(using InvariantK[F]): F[[a] =>> Annotation[G[a]]] =
    fg.imapK[[a] =>> Annotation[G[a]]]([A] => (self: G[A]) => Annotation(self))([A] =>
      (annotation: Annotation[G[A]]) => annotation.self
    )

  given operation2[F[_[_], _[_]], G[_], H[_]](using
      fgh: F[G, H]
  )(using InvariantK[[f[_]] =>> F[f, H]]): F[[a] =>> Annotation[G[a]], H] =
    fgh.imapK[[a] =>> Annotation[G[a]]]([A] => (self: G[A]) => Annotation(self))([A] =>
      (annotation: Annotation[G[A]]) => annotation.self
    )
