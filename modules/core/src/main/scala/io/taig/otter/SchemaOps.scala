package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain

trait SchemaOps[F[_, _, _], G[_, _, _], C[_, _, _], T[_, _, _]]:
  extension [M, A, B](self: F[M, A, B])
    def collection: C[M, self.type, Vector[B]]
    final def collection[T[_]](builder: CollectionBuilder[T])(using
        SchemaInvariant[C[M, self.type, *]]
    ): C[M, self.type, T[B]] = collection.imap(builder.to)(builder.from)
    def modify[N](f: M => N): F[N, A, B]
    def optional: G[M, A, Option[B]]
    def toTuple: T[M, self.type, B]

trait CollectionOps[F[_, _, _], T[_, _, _]] extends SchemaOps[F, F, F, T]:
  extension [M, A, B](self: F[M, A, B]) def schema: Schema[M, ?, ?]

trait PrimitiveOps[F[_, _], G[_, _], C[_, _, _], T[_, _, _]]
    extends SchemaOps[[m, _, a] =>> F[m, a], [m, _, a] =>> G[m, a], C, T]:
  extension [M, A](self: F[M, A]) def tpe: Type[?]

trait TupleOps[F[_, _, _], C[_, _, _]] extends SchemaOps[F, F, C, F]:
  extension [M, A, B](self: F[M, A, B]) def schemas: Chain[Schema[M, ?, ?]]
