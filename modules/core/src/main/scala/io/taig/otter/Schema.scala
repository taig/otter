package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation
import cats.Contravariant
import cats.Functor

type Schema[+A, B] = Primitive[B] | Tuple[A, B]

object Schema:
  type Reader[+A, +B] = Primitive.Reader[B] | Tuple.Reader[A, B]

  type Writer[+A, -B] = Primitive.Writer[B] | Tuple.Writer[A, B]

trait SchemaInvariant[F[_, _], G[a, b] >: F[a, b]]:
  def invariant[A]: Invariant[F[A, *]] = new Invariant[F[A, *]]:
    override def imap[B, C](fa: F[A, B])(f: B => C)(g: C => B): F[A, C] =
      ??? // self.ivalidate(fa)(Validation.lift(f))(g)

  // def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D]

  // def optional[A](fa: F[A]): G[Option[A]]

  extension [A, B](self: F[A, B]) def toTuple: Tuple[F[A, B], B]

// trait SchemaContravariant[F[_], G[a] >: F[a]] extends Contravariant[F], SchemaInvariant[F, G]:
//   override def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D] =
//     contramap(fa)(f)

// trait SchemaFunctor[F[_], G[a] >: F[a]] extends Functor[F], SchemaInvariant[F, G]:
//   def validate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D]): F[D]

//   override def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D] =
//     validate(fa)(validation)

//   override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))
