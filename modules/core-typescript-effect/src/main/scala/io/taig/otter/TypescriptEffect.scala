package io.taig.otter

import cats.syntax.all.*
import cats.derived.*
import cats.Order

final case class TypescriptEffect(typescript: Option[Typescript.Value], effect: Effect[TypescriptEffect]) derives Order:
  def definition(name: String): TypescriptEffectDefinition = TypescriptEffectDefinition(name, value = this)

  def isRecursive: Boolean = effect.isRecursion || effect.exists(_.effect.isRecursion)

  def toEffect: Effect.Value = Effect.Value(effect.map(_.toEffect))

object TypescriptEffect:
  def apply(effect: Effect[TypescriptEffect]): TypescriptEffect = TypescriptEffect(typescript = none, effect = effect)
