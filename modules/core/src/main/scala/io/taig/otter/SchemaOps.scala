package io.taig.otter

import cats.data.Chain

trait SchemaOps[F[_, _], G[_, _], C[_, _], T[_, _]]:
  extension [A, B](self: F[A, B])
    def collection: C[self.type, Vector[B]]
    def optional: G[A, Option[B]]
    def tuple: T[self.type, B]

trait CollectionOps[F[_, _], T[_, _], S, CB[_[_]]] extends SchemaOps[F, F, F, T]:
  extension [A, B](self: F[A, B]) def schema: S
  extension [A, B](self: F[A, Vector[B]]) def apply[T[_]](builder: CB[T]): F[A, T[B]]

trait PrimitiveOps[F[_], G[_], C[_, _], T[_, _]] extends SchemaOps[[_, a] =>> F[a], [_, a] =>> G[a], C, T]:
  extension [A](self: F[A]) def tpe: Type[?]

trait TupleOps[F[_, _], C[_, _], S] extends SchemaOps[F, F, C, F]:
  extension [A, B](self: F[A, B]) def schemas: Chain[S]
