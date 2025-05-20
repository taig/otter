package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedEnumerationSchema[Self[_], Value[_]](
    enumeration: EnumerationSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends EnumerationSchema[Self, Value],
      EnrichedSchema[Self]:
  export enumeration.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedEnumerationSchema[T, Value] =
    new EnrichedEnumerationSchema(enumeration = enumeration.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedEnumerationSchema:
  def apply[Self[_], Value[_]](using
      enumeration: EnumerationSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedEnumerationSchema[Enriched[Self, *], Value] = new EnrichedEnumerationSchema[Enriched[Self, *], Value](
    enumeration = enumeration.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
