package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedRecordSchema[Self[_], -Field[_]] extends RecordSchema[Self, Field], EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedRecordSchema[T, Field] =
    new EnrichedRecordSchema[T, Field]:
      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))

      extension [A](ta: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedRecordSchema:
  inline def apply[Self[_], Field[_]](using
      schema: EnrichedRecordSchema[Self, Field]
  ): EnrichedRecordSchema[Self, Field] = schema

  given [Self[_], Field[_]](using
      self: RecordSchema[Self, Field],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedRecordSchema[Enrichment[Self, *], Field] =
    val record: RecordSchema[Enrichment[Self, *], Field] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedRecordSchema[Enrichment[Self, *], Field]:
      export record.{lift, zip}
      export enrichment.{imap, metadata}
