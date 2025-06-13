package io.taig.otter.syntax

import io.taig.otter.Effect
import io.taig.otter.EffectKeys
import io.taig.otter.operation.Enriched
import io.taig.otter.syntax.EnrichedSyntax.*

trait EnrichedEffectSyntax:
  extension [A](a: A)(using enriched: Enriched[A])
    def effect(value: Effect.Value): A = a.metadata(EffectKeys.effect, value)
    def effect(value: String): A = effect(Effect.Value(Effect.Dynamic(value)))

object EnrichedEffectSyntax extends EnrichedEffectSyntax
