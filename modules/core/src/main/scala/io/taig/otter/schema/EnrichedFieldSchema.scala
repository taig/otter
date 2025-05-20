package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedFieldSchema[Self[_], Key[_], Value[_]](
    field: FieldSchema[Self, Key, Value],
    enriched: EnrichedSchema[Self]
) extends FieldSchema[Self, Key, Value],
      EnrichedSchema[Self]:
  export field.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedFieldSchema[T, Key, Value] =
    new EnrichedFieldSchema(field = field.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedFieldSchema:
  def apply[Self[_], Key[_], Value[_]](using
      field: FieldSchema[Self, Key, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedFieldSchema[Enriched[Self, *], Key, Value] =
    new EnrichedFieldSchema[Enriched[Self, *], Key, Value](
      field = field.imapK(
        [A] => (self: Self[A]) => Enriched(self)
      )([A] => (enriched: Enriched[Self, A]) => enriched.self),
      enriched
    )
