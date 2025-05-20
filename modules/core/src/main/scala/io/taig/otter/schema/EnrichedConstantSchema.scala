package io.taig.otter.schema

import io.taig.otter.Enriched

final class EnrichedConstantSchema[Self[_], -Value[_]](
    constant: ConstantSchema[Self, Value],
    enriched: EnrichedSchema[Self]
) extends ConstantSchema[Self, Value],
      EnrichedSchema[Self]:
  export constant.{imap as _, imapK as _, *}
  export enriched.{imapK as _, *}

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedConstantSchema[T, Value] =
    new EnrichedConstantSchema(constant = constant.imapK(fK)(gK), enriched = enriched.imapK(fK)(gK))

object EnrichedConstantSchema:
  def apply[Self[_], Value[_]](using
      constant: ConstantSchema[Self, Value],
      enriched: EnrichedSchema[Enriched[Self, *]]
  ): EnrichedConstantSchema[Enriched[Self, *], Value] = new EnrichedConstantSchema[Enriched[Self, *], Value](
    constant = constant.imapK(
      [A] => (self: Self[A]) => Enriched(self)
    )([A] => (enriched: Enriched[Self, A]) => enriched.self),
    enriched
  )
