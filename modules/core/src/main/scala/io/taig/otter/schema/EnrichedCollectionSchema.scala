package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedCollectionSchema[Self[_], -Value[_]](
    collection: CollectionSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends CollectionSchema[Self, Value],
      EnrichedSchema[Self]:
  export collection.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedCollectionSchema[T, Value] =
    new EnrichedCollectionSchema(collection = collection.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedCollectionSchema:
  def apply[Self[_], Value[_]](using
      collection: CollectionSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedCollectionSchema[Enriched[Self, *], Value] = new EnrichedCollectionSchema[Enriched[Self, *], Value](
    collection = collection.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
