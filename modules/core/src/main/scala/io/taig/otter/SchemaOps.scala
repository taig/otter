package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain

trait SchemaOps[F[_, _], G[_, _], C[_, _], T[_, _]]:
  extension [A, B](self: F[A, B])
    def collection: C[self.type, Vector[B]]
    final def collection[T[_]](builder: CollectionBuilder[T])(using
        SchemaInvariant[C[self.type, *]]
    ): C[self.type, T[B]] = collection.imap(builder.to)(builder.from)
    def optional: G[A, Option[B]]
    def toTuple: T[self.type, B]

trait CollectionOps[F[_, _], T[_, _], S] extends SchemaOps[F, F, F, T]:
  extension [A, B](self: F[A, B]) def schema: S

trait PrimitiveOps[F[_], G[_], C[_, _], T[_, _]] extends SchemaOps[[_, a] =>> F[a], [_, a] =>> G[a], C, T]:
  extension [A](self: F[A]) def tpe: Type[?]

trait TupleOps[F[_, _], C[_, _], S] extends SchemaOps[F, F, C, F]:
  extension [A, B](self: F[A, B]) def schemas: Chain[S]
