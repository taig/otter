package io.taig.otter

import cats.data.NonEmptyList

trait EffectKeys:
  val effect: Metadata.Key[Effect[Effect.Value]] = Metadata.Key("effect")

object EffectKeys extends EffectKeys
