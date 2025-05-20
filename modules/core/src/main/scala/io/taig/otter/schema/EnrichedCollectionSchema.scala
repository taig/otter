package io.taig.otter.schema

final class EnrichedCollectionSchema[Self[_], -Value[_]](collection: CollectionSchema[Self, Value], enriched: EnrichedSchema[Self]) extends CollectionSchema[Self, Value], EnrichedSchema[Self]:
  export collection.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedCollectionSchema[T, Value] = 
    new EnrichedCollectionSchema(collection = collection.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))