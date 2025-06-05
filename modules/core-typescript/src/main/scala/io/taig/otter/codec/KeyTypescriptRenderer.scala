package io.taig.otter.codec

import cats.Id
import io.taig.otter.Key
import io.taig.otter.Typescript
import cats.Order

final class KeyTypescriptRenderer[A: Order](renderer: Renderer[Key, A]) extends Renderer[Key, Typescript[A]]:
  val constant = ConstantTypescriptRenderer[Key, Id](printer = KeyPrinter.Quoted)

  val enumeration = EnumerationTypescriptRenderer[Key](printer = KeyPrinter.Quoted)

  val union = UnionTypescriptRenderer[Key, Id, A](renderer)

  override def render[B](schema: Key[B]): Typescript[A] = schema match
    case Key.Primitive.String(self) => PrimitiveTypescriptRenderer.render(self)
    case Key.Constant(self)         => constant.render(schema = self)
    case Key.Enumeration(self)      => enumeration.render(schema = self)
    case Key.Union(self)            => union.render(schema = self)
