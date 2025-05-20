package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedPrimitiveSchema[Self[_]](primitive: PrimitiveSchema[Self], enriched: EnrichedSchema[Self])
    extends PrimitiveSchema[Self],
      EnrichedSchema[Self]:
  export primitive.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedPrimitiveSchema[T] =
    new EnrichedPrimitiveSchema(primitive = primitive.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedPrimitiveSchema:
  def apply[Self[_]](using
      primitive: PrimitiveSchema[Self],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedPrimitiveSchema[Enriched[Self, *]] = new EnrichedPrimitiveSchema[Enriched[Self, *]](
    primitive = primitive.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
