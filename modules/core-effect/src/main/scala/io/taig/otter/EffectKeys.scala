package io.taig.otter

trait EffectKeys:
  val effect: Metadata.Key[Effect[Nothing]] = Metadata.Key("effect")

object EffectKeys extends EffectKeys
