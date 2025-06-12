package io.taig.otter

import cats.syntax.all.*

final case class TypescriptEffect(typescript: Option[Typescript.Value], effect: Effect[TypescriptEffect]):
  def definition(name: String): TypescriptEffectDefinition = TypescriptEffectDefinition(name, value = this)

object TypescriptEffect:
  def apply(effect: Effect[TypescriptEffect]): TypescriptEffect = TypescriptEffect(typescript = none, effect = effect)
