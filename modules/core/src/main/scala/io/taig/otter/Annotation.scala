package io.taig.otter

import cats.Applicative
import cats.arrow.Profunctor

/** Attaches [[Metadata]] to a schema node. */
final case class Annotation[+A](metadata: Metadata, self: A):
  def map[B](f: A => B): Annotation[B] = copy(self = f(self))

object Annotation:
  def apply[A](self: A): Annotation[A] = Annotation(metadata = Metadata.Empty, self)

  given applicative: Applicative[Annotation]:
    override def map[A, B](fa: Annotation[A])(f: A => B): Annotation[B] = fa.map(f)

    override def ap[A, B](ff: Annotation[A => B])(fa: Annotation[A]): Annotation[B] =
      Annotation(metadata = fa.metadata ++ ff.metadata, self = ff.self(fa.self))

    override def pure[A](a: A): Annotation[A] = Annotation(self = a)

  given annotated: [A] => Annotated[Annotation[A]]:
    extension (self: Annotation[A])
      override def lens: (Metadata, Metadata => Annotation[A]) =
        (self.metadata, metadata => self.copy(metadata = metadata))

  /** Lifts a schema node's [[Profunctor]] through the annotation. */
  given profunctor: [F[- _, + _]] => (F: Profunctor[F]) => Profunctor[[w, r] =>> Annotation[F[w, r]]]:
    override def dimap[W0, R0, W, R](
        fab: Annotation[F[W0, R0]]
    )(f: W => W0)(g: R0 => R): Annotation[F[W, R]] = fab.map(F.dimap(_)(f)(g))
