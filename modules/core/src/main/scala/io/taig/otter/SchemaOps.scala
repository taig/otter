package io.taig.otter

import cats.data.Chain
import cats.Functor
import cats.Invariant
import cats.Contravariant
import io.taig.otter.validation.Validation

trait SchemaOps[F[_, _], G[_, _], C[_, _]]:
  extension [A, B](self: F[A, B])
    def collection: C[self.type, Vector[B]]
    def optional: G[self.type, Option[B]]

trait ValidationIsomorphicOps[F[_, _]]:
  extension [A, B](self: F[A, B])
    def ivalidate[C](validation: Validation[B, ?, ?, C])(f: C => B): F[A, C]
    def ivalidate_(validation: Validation[B, ?, ?, Unit]): F[A, B] = ivalidate(validation.tap)(identity)

trait ValidationReaderOps[F[_, _]]:
  extension [A, B](self: F[A, B])
    def validate[C](validation: Validation[B, ?, ?, C]): F[A, C]
    def validate_(validation: Validation[B, ?, ?, Unit]): F[A, B] = validate(validation.tap)

trait SchemaIsomorphicOps[F[_, _], G[_, _], C[_, _]] extends SchemaOps[F, G, C]:
  def invariant[A]: Invariant[F[A, *]]

  extension [A, B](self: F[A, B]) def iso: F[A, B]

trait SchemaReaderOps[F[_, _], G[_, _], C[_, _]] extends SchemaOps[F, G, C]:
  def functor[A]: Functor[F[A, *]]

  extension [A, B](self: F[A, B]) def reader: F[A, B]

trait SchemaWriterOps[F[_, _], G[_, _], C[_, _]] extends SchemaOps[F, G, C]:
  def contravariant[A]: Contravariant[F[A, *]]

  extension [A, B](self: F[A, B]) def writer: F[A, B]

trait PrimitiveOps[F[_], G[_], C[_, _]] extends SchemaOps[[_, a] =>> F[a], [_, a] =>> G[a], C]

trait PrimitiveIsomorphicOps[F[_], G[_], C[_, _]]
    extends SchemaIsomorphicOps[[_, a] =>> F[a], [_, a] =>> G[a], C],
      PrimitiveOps[F, G, C],
      ValidationIsomorphicOps[[_, a] =>> F[a]]

trait PrimitiveReaderOps[F[_], G[_], C[_, _]]
    extends SchemaReaderOps[[_, a] =>> F[a], [_, a] =>> G[a], C],
      PrimitiveOps[F, G, C],
      ValidationReaderOps[[_, a] =>> F[a]]

trait PrimitiveWriterOps[F[_], G[_], C[_, _]]
    extends SchemaWriterOps[[_, a] =>> F[a], [_, a] =>> G[a], C],
      PrimitiveOps[F, G, C]

// trait SchemaOps[F[_, _], G[_, _], C[_, _]]:
//   extension [A, B](self: F[A, B])
//     def collection: C[self.type, Vector[B]]
//     def optional: G[A, Option[B]]
// def tuple: T[self.type, B]

// trait CollectionOps[F[_, _], T[_, _], S, CB[_, _]] extends SchemaOps[F, F, F, T]:
//   extension [A, B](self: F[A, B]) def schema: S
//   extension [A, B](self: F[A, Vector[B]]) def apply[C](builder: CB[Vector[B], C]): F[A, C]

// trait PrimitiveOps[F[_], G[_], C[_, _]] extends SchemaOps[[_, a] =>> F[a], [_, a] =>> G[a], C]:
//   extension [A](self: F[A]) def tpe: Type[?]

// trait TupleOps[F[_, _], C[_, _], S] extends SchemaOps[F, F, C, F]:
//   extension [A, B](self: F[A, B]) def schemas: Chain[S]
