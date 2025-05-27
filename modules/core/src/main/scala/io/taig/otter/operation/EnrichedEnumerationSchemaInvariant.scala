package io.taig.otter.operation

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import io.taig.otter.Reference

trait EnrichedEnumerationSchemaInvariant[Self[_], Value[_]]
    extends EnumerationSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedEnumerationSchemaInvariant[T, Value] = new EnrichedEnumerationSchemaInvariant[T, Value]:
    override def apply[A, B](schema: => Value[A], mapping: Mapping[B, A]): T[B] = fK(self(schema, mapping))

    extension [A](ta: T[A])
      override def schema: Reference[Value, ?] = self.schema(gK(ta))
      override def values: NonEmptyList[A] = self.values(gK(ta))
      override def metadata: Metadata = self.metadata(gK(ta))
      override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

    override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedEnumerationSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedEnumerationSchemaInvariant[Self, Value]
  ): EnrichedEnumerationSchemaInvariant[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: EnumerationSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedEnumerationSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
    val enumeration: EnumerationSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedEnumerationSchemaInvariant[[a] =>> Enrichment[Self[a]], Value]:
      export enumeration.{apply, schema, values}
      export enrichment.{imap, metadata}
