package io.taig.otter.operation

import io.taig.otter.Enrichment
import io.taig.otter.Metadata

trait EnrichedRecordSchemaInvariant[Self[_], -Field[_]]
    extends RecordSchemaInvariant[Self, Field],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedRecordSchemaInvariant[T, Field] =
    new EnrichedRecordSchemaInvariant[T, Field]:
      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))

      extension [A](ta: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedRecordSchemaInvariant:
  inline def apply[Self[_], Field[_]](using
      schema: EnrichedRecordSchemaInvariant[Self, Field]
  ): EnrichedRecordSchemaInvariant[Self, Field] = schema

  given [Self[_], Field[_]](using
      self: RecordSchemaInvariant[Self, Field],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedRecordSchemaInvariant[[a] =>> Enrichment[Self[a]], Field] =
    val record: RecordSchemaInvariant[[a] =>> Enrichment[Self[a]], Field] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedRecordSchemaInvariant[[a] =>> Enrichment[Self[a]], Field]:
      export record.{lift, zip}
      export enrichment.{imap, metadata}
