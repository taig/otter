package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedUnionSchema[Self[_], Value[_]](
    union: UnionSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends UnionSchema[Self, Value],
      EnrichedSchema[Self]:
  export union.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedUnionSchema[T, Value] =
    new EnrichedUnionSchema(union = union.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedUnionSchema:
  def apply[Self[_], Value[_]](using
      union: UnionSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedUnionSchema[Enriched[Self, *], Value] = new EnrichedUnionSchema[Enriched[Self, *], Value](
    union = union.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
