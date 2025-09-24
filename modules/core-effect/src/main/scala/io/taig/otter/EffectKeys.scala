package io.taig.otter


trait EffectKeys:
  val effect: Metadata.Key[Effect[Effect.Value]] = Metadata.Key("effect")

object EffectKeys extends EffectKeys
