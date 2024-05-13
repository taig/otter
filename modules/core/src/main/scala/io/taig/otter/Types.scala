package io.taig.otter

import io.taig.otter as Base

trait Types[S[+_]]:
  final type Schema[A] = Schema.Of[Any, A]

  object Schema:
    type Of[+A, B] = S[Base.Schema[S, A, B]]

    type Writer[-A] = Writer.Of[Any, A]

    object Writer:
      type Of[+A, -B] = S[Base.Schema.Writer[S, A, B]]

  type Primitive[A] = S[Base.Primitive[A]]

  object Primitive:
    type Writer[-A] = S[Base.Primitive.Writer[A]]

  final type Tuple[A] = Tuple.Of[Any, A]

  object Tuple:
    type Of[+A, B] = S[Base.Tuple[S, A, B]]

    type Writer[-A] = Writer.Of[Any, A]

    object Writer:
      type Of[+A, -B] = S[Base.Tuple.Writer[S, A, B]]

  // trait SchemaInvariant[F[_]]:
  //   // def invariant[A]: Invariant[F[*]] = new Invariant[F[A, *]]:
  //   //   override def imap[B, C](fa: F[A, B])(f: B => C)(g: C => B): F[A, C] =
  //   //     ??? // ivalidate(fa)(Validation.lift(f))(g)

  //   //   def ivalidate[A, B, C, D, E](fab: F[A, B])(validation: SchemaValidation[B, C, D, E])(f: E => B): F[A, E]

  //   extension [A](self: F[A]) def toTuple: Tuple[A]
