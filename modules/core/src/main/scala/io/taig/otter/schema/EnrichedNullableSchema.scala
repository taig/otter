package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedNullableSchema[Self[_], -Value[_]](
    nullable: NullableSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends NullableSchema[Self, Value],
      EnrichedSchema[Self]:
  export nullable.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedNullableSchema[T, Value] =
    new EnrichedNullableSchema(nullable = nullable.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedNullableSchema:
  def apply[Self[_], Value[_]](using
      nullable: NullableSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedNullableSchema[Enriched[Self, *], Value] = new EnrichedNullableSchema[Enriched[Self, *], Value](
    nullable = nullable.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
