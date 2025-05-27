package io.taig.otter.operation

import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import cats.syntax.all.*

trait EnrichedCollectionSchemaInvariant[Self[_], -Value[_]]
    extends CollectionSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedCollectionSchemaInvariant[T, Value] =
    new EnrichedCollectionSchemaInvariant[T, Value]:
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

object EnrichedCollectionSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedCollectionSchemaInvariant[Self, Value]
  ): EnrichedCollectionSchemaInvariant[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: CollectionSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedCollectionSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
    val collection: CollectionSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedCollectionSchemaInvariant[[a] =>> Enrichment[Self[a]], Value]:
      export collection.{indexed, linked}
      export enrichment.{imap, metadata}
