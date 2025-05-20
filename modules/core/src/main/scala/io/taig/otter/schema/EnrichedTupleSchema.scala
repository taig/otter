package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedTupleSchema[Self[_], -Value[_]](
    tuple: TupleSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends TupleSchema[Self, Value],
      EnrichedSchema[Self]:
  export tuple.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedTupleSchema[T, Value] =
    new EnrichedTupleSchema(tuple = tuple.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedTupleSchema:
  def apply[Self[_], Value[_]](using
      tuple: TupleSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedTupleSchema[Enriched[Self, *], Value] = new EnrichedTupleSchema[Enriched[Self, *], Value](
    tuple = tuple.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
