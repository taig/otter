package io.taig.otter

import io.taig.otter as Base
import cats.Invariant

trait Types[F[+_]]:
  final type Schema[B] = Schema.Of[Any, B]

  object Schema:
    final type Of[+A, B] = F[Base.Schema[F, A, B]]

    type Reader[+B] = Reader.Of[Any, B]

    object Reader:
      type Of[+A, +B] = F[Base.Schema.Reader[F, A, B]]

    type Writer[-B] = Writer.Of[Any, B]

    object Writer:
      type Of[+A, -B] = F[Base.Schema.Writer[F, A, B]]

  final type Primitive[A] = F[Base.Primitive[A]]

  object Primitive:
    type Required[A] = F[Base.Primitive.Required[A]]

    object Required:
      type Reader[+A] = F[Base.Primitive.Required.Reader[A]]
      type Writer[-A] = F[Base.Primitive.Required.Writer[A]]

    type Reader[+A] = F[Base.Primitive.Reader[A]]
    type Writer[-A] = F[Base.Primitive.Writer[A]]

  final type Tuple[B] = Tuple.Of[Any, B]

  object Tuple:
    final type Of[+A, B] = F[Base.Tuple[F, A, B]]

    type Reader[+A] = Reader.Of[Any, A]

    object Reader:
      type Of[+A, +B] = F[Base.Tuple.Reader[F, A, B]]

    type Writer[-A] = Writer.Of[Any, A]

    object Writer:
      type Of[+A, -B] = F[Base.Tuple.Writer[F, A, B]]

  trait SchemaInvariant[F[_, _]]:
    def invariant[A]: Invariant[F[A, *]] = new Invariant[F[A, *]]:
      override def imap[B, C](fa: F[A, B])(f: B => C)(g: C => B): F[A, C] =
        ??? // ivalidate(fa)(Validation.lift(f))(g)

    //   def ivalidate[A, B, C, D, E](fab: F[A, B])(validation: SchemaValidation[B, C, D, E])(f: E => B): F[A, E]

    extension [A, B](self: F[A, B]) def toTuple: Tuple.Of[F[A, B], B]
