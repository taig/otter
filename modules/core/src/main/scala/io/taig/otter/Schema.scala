package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation
import cats.Contravariant
import cats.Functor

type Schema[+A, B] = Tuple[A, B]

object Schema:
  type Reader[+A, +B] = Tuple.Reader[A, B]

  type Writer[+A, -B] = Tuple.Writer[A, B]

trait SchemaInvariant[F[_], G[a] >: F[a]] extends Optional[F, G]:
  self =>
  def invariant[A]: Invariant[F] = new Invariant[F] {
    override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
      self.ivalidate(fa)(Validation.lift(f))(g)
  }
  def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D]

trait SchemaContravariant[F[_], G[a] >: F[a]] extends Contravariant[F], SchemaInvariant[F, G]:
  override def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D] =
    contramap(fa)(f)

trait SchemaFunctor[F[_], G[a] >: F[a]] extends Functor[F], SchemaInvariant[F, G]:
  def validate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D]): F[D]

  override def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D] =
    validate(fa)(validation)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))
