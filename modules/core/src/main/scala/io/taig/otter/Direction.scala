package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor

/** Views of a schema from one direction.
  *
  * A schema is a profunctor, so a `Functor` over it can only exist once the write side is out of the way, and a
  * `Contravariant` only once the read side is. Instantiating the other slot to its maximal element is what does that:
  * nothing can be encoded through `Nothing` and nothing usable decoded out of `Any`, which is exactly what
  * `Json.Reader` and `Json.Writer` already say.
  *
  * This is not [[Side]], which is the runtime choice an interpreter that involves no value makes about which of a
  * schema's two wire shapes it describes.
  */
object Direction:
  /** The read side of a schema that writes nothing. */
  def functor[F[-_, +_]](using P: Profunctor[F]): Functor[[a] =>> F[Nothing, a]] = new Functor[[a] =>> F[Nothing, a]]:
    override def map[A, B](fa: F[Nothing, A])(f: A => B): F[Nothing, B] = P.rmap(fa)(f)

  /** The write side of a schema that reads nothing usable. */
  def contravariant[F[-_, +_]](using P: Profunctor[F]): Contravariant[[a] =>> F[a, Any]] =
    new Contravariant[[a] =>> F[a, Any]]:
      override def contramap[A, B](fa: F[A, Any])(f: B => A): F[B, Any] = P.lmap(fa)(f)

  /** Both sides of a schema that round trips, which can only move in lockstep. */
  def invariant[F[-_, +_]](using P: Profunctor[F]): Invariant[[a] =>> F[a, a]] = new Invariant[[a] =>> F[a, a]]:
    override def imap[A, B](fa: F[A, A])(f: A => B)(g: B => A): F[B, B] = P.dimap(fa)(g)(f)
