package io.taig.otter

trait EffectKeys:
  val effect: Metadata.Key[Effect.Value] = Metadata.Key("effect")

object EffectKeys extends EffectKeys
