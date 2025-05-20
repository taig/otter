package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Enriched

final class EnrichedRecordSchema[Self[_], -Field[_]](
    record: RecordSchema[Self, Field],
    enriched: EnrichedSchema[Self]
) extends RecordSchema[Self, Field],
      EnrichedSchema[Self]:
  export record.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedRecordSchema[T, Field] =
    new EnrichedRecordSchema[T, Field](record.imapK(fK)(gK), enriched.imapK(fK)(gK))

object EnrichedRecordSchema:
  def apply[Self[_], Field[_]](using
      record: RecordSchema[Self, Field],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedRecordSchema[Enriched[Self, *], Field] = new EnrichedRecordSchema[Enriched[Self, *], Field](
    record = record.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
