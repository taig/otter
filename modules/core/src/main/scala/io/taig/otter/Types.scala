package io.taig.otter

import cats.Invariant
import io.taig.otter as Base
import io.taig.otter.validation.Validation
import cats.data.Chain
import io.taig.otter.validation.Constraint
import cats.Contravariant
import cats.Functor

trait Types[S[+_]]:
  final type Schema[A] = Schema.Of[Any, A]

  object Schema:
    type Of[+A, B] = S[Base.Schema[S, A, B]]

    type Reader[+A] = Reader.Of[Any, A]

    object Reader:
      type Of[+A, +B] = S[Base.Schema.Reader[S, A, B]]

    type Writer[-A] = Writer.Of[Any, A]

    object Writer:
      type Of[+A, -B] = S[Base.Schema.Writer[S, A, B]]

  type Primitive[A] = S[Base.Primitive[A]]

  object Primitive:
    type Writer[-A] = S[Base.Primitive.Writer[A]]

  final type Tuple[A] = Tuple.Of[Schema[Any], A]

  object Tuple:
    type Of[+A, B] = S[Base.Tuple[S, A, B]]

    type Reader[+A] = Reader.Of[Schema.Reader[Any], A]

    object Reader:
      type Of[+A, +B] = S[Base.Tuple.Reader[S, A, B]]

    type Writer[-A] = Writer.Of[Schema.Writer[Any], A]

    object Writer:
      type Of[+A, -B] = S[Base.Tuple.Writer[S, A, B]]

  trait SchemaFunctor[F[_, _], G[_, _]]:
    def functor[A]: Functor[F[A, *]] = new Functor[F[A, *]]:
      override def map[B, C](fa: F[A, B])(f: B => C): F[A, C] = validate(fa)(Validation.lift(f))

    extension [A, B](fab: F[A, B])
      def optional: G[A, Option[B]]
      def toTuple: Tuple.Writer.Of[F[A, B], B]
      def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): F[A, E]

  trait SchemaContravariant[F[_, _], G[_, _]]:
    def contravariant[A]: Contravariant[F[A, *]]

    extension [A, B](fab: F[A, B])
      // override def ivalidate[C, D, E](validation: SchemaValidation[B, C, D, E])(f: E => B): F[A, E] =
      //   contravariant[A].contramap(fab)(f)
      def optional: G[A, Option[B]]
      def toTuple: Tuple.Writer.Of[F[A, B], B]

  trait SchemaInvariant[F[_, _], G[_, _]] extends SchemaFunctor[F, G], SchemaContravariant[F, G]:
    def invariant[A]: Invariant[F[A, *]] = new Invariant[F[A, *]]:
      override def imap[B, C](fa: F[A, B])(f: B => C)(g: C => B): F[A, C] =
        ivalidate(fa)(Validation.lift(f))(g)

    extension [A, B](fab: F[A, B])
      def constraints: Chain[Constraint[?]]
      def ivalidate[C, D, E](validation: SchemaValidation[B, C, D, E])(f: E => B): F[A, E]
      def optional: G[A, Option[B]]
      override def toTuple: Tuple.Of[F[A, B], B]
