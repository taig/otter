package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.Id

type Schema[+S[_], +A, B] = Primitive[B] | Tuple[S, A, B]

object Schema:
  type Reader[+S[_], +A, +B] = Primitive.Reader[B] | Tuple.Reader[S, A, B]

  type Writer[+S[_], +A, -B] = Primitive.Writer[B] | Tuple.Writer[S, A, B]

// trait SchemaInvariant[F[_[_], _, _]]:
//   def invariant[S[_], A]: Invariant[F[S, A, *]] = new Invariant[F[S, A, *]]:
//     override def imap[B, C](fa: F[S, A, B])(f: B => C)(g: C => B): F[S, A, C] =
//       ??? // ivalidate(fa)(Validation.lift(f))(g)

// //   def ivalidate[A, B, C, D, E](fab: F[A, B])(validation: SchemaValidation[B, C, D, E])(f: E => B): F[A, E]

//   extension [S[_], A, B](self: F[S, A, B]) def toTuple: S[Tuple[S, F[S, A, B], B]]

// trait SchemaContravariant[F[_, _]] extends SchemaInvariant[F]:
//   def contravariant[A]: Contravariant[F[A, *]]

//   override def ivalidate[A, B, C, D, E](fab: F[A, B])(validation: Validation[B, C, D, E])(f: E => B): F[A, E] =
//     contravariant[A].contramap(fab)(f)

// trait SchemaFunctor[F[_, _]] extends SchemaInvariant[F]:
//   def functor[A]: Functor[F[A, *]] = new Functor[F[A, *]]:
//     override def map[B, C](fa: F[A, B])(f: B => C): F[A, C] = validate(fa)(Validation.lift(f))

//   def validate[A, B, C, D, E](fab: F[A, B])(validation: SchemaValidation[B, C, D, E]): F[A, E]

//   override def ivalidate[A, B, C, D, E](fab: F[A, B])(validation: Validation[B, C, D, E])(f: E => B): F[A, E] =
//     validate(fab)(validation)
