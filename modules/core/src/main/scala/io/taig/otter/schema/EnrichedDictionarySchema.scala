package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedDictionarySchema[Self[_], -Key[_], -Value[_]](
    dictionary: DictionarySchema[Self, Key, Value],
    enriched: EnrichedSchema[Self]
) extends DictionarySchema[Self, Key, Value],
      EnrichedSchema[Self]:
  export dictionary.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedDictionarySchema[T, Key, Value] =
    new EnrichedDictionarySchema(dictionary = dictionary.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedDictionarySchema:
  def apply[Self[_], Key[_], Value[_]](using
      dictionary: DictionarySchema[Self, Key, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedDictionarySchema[Enriched[Self, *], Key, Value] =
    new EnrichedDictionarySchema[Enriched[Self, *], Key, Value](
      dictionary = dictionary.imapK(
        [A] => (self: Self[A]) => Enriched(self)
      )([A] => (enriched: Enriched[Self, A]) => enriched.self),
      enriched
    )
