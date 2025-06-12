package io.taig.otter.codec

import io.taig.otter.Effect
import io.taig.otter.Key
import cats.Id
import cats.Order
import io.taig.otter.EffectState
import cats.data.State

object KeyEffectRenderer extends Renderer[Key, EffectState[Effect.Value]]:
  val constant = ConstantEffectRenderer(printer = KeyPrinter.Quoted)
  val enumeration = EnumerationEffectRenderer(
    printer = KeyPrinter.Quoted.map(value => Effect.Value(Effect.Literal(value)))
  )
  val primitive = PrimitiveEffectRenderer.map(Effect.Value.apply)
  val union = UnionEffectRenderer[Key, EffectState, Effect.Value](renderer = this).map(_.map(Effect.Value.apply))

  override def render[B](schema: Key[B]): EffectState[Effect.Value] = schema match
    case Key.Primitive.String(self) => State.pure(primitive.render(self))
    case Key.Constant(self)         => ??? // constant.render(schema = self)
    case Key.Enumeration(self)      => ??? // enumeration.render(schema = self)
    case Key.Union(self)            => union.render(schema = self)
