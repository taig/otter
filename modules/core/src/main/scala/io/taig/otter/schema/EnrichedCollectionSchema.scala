package io.taig.otter.schema

import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import cats.syntax.all.*

trait EnrichedCollectionSchema[Self[_], -Value[_]] extends CollectionSchema[Self, Value], EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedCollectionSchema[T, Value] =
    new EnrichedCollectionSchema[T, Value]:
      override def linked[A](
          schema: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          unique: Boolean
      ): T[List[A]] =
        fK(self.linked(schema, minimum, maximum, unique))

      override def indexed[A](
          schema: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          unique: Boolean
      ): T[Vector[A]] =
        fK(self.indexed(schema, minimum, maximum, unique))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] =
        fK(self.imap(gK(ta))(f)(g))

object EnrichedCollectionSchema:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedCollectionSchema[Self, Value]
  ): EnrichedCollectionSchema[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: CollectionSchema[Self, Value],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedCollectionSchema[Enrichment[Self, *], Value] =
    val collection: CollectionSchema[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedCollectionSchema[Enrichment[Self, *], Value]:
      export collection.{indexed, linked}
      export enrichment.{imap, metadata}
