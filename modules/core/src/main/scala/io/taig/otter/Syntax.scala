package io.taig.otter

import io.taig.otter as Base

trait Ops extends Types:
  trait SchemaOps[F[_, _], G[_, _]]:
    extension [A, B](self: F[A, B]) def optional: G[A, Option[B]]

  trait SchemaIsomorphicOps[F[_, _], G[_, _]] extends SchemaOps[F, G]:
    extension [A, B](self: F[A, B]) def collection: Collection.Of[A, Vector[B]]

  trait SchemaReaderOps[F[_, _], G[_, _]] extends SchemaOps[F, G]:
    extension [A, B](self: F[A, B]) def collection: Collection.Reader.Of[A, Vector[B]]

  trait SchemaWriterOps[F[_, _], G[_, _]] extends SchemaOps[F, G]:
    extension [A, B](self: F[A, B])
      def collection: Collection.Writer.Of[A, Vector[B]]
      def contramap[C](f: C => B): F[A, C]

  trait PrimitiveOps[F[_, _], G[_, _]] extends SchemaOps[F, G]:
    def tpe: Type[?]

  trait PrimitiveIsomorphicOps[F[_, _], G[_, _]] extends PrimitiveOps[F, G], SchemaIsomorphicOps[F, G]

  trait PrimitiveReaderOps[F[_, _], G[_, _]] extends PrimitiveOps[F, G], SchemaReaderOps[F, G]

  trait PrimitiveWriterOps[F[_, _], G[_, _]] extends PrimitiveOps[F, G], SchemaWriterOps[F, G]

trait Syntax extends Syntax1

trait Syntax1 extends Syntax2

trait Syntax2 extends Ops
