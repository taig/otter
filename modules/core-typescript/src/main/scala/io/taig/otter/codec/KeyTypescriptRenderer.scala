package io.taig.otter.codec

import cats.Id
import io.taig.otter.Key
import io.taig.otter.Typescript

object KeyTypescriptRenderer extends Renderer[Key, Typescript.Value]:
  val constant = ConstantTypescriptRenderer[Key](printer = KeyPrinter.Quoted)
  val enumeration = EnumerationTypescriptRenderer[Key](printer = KeyPrinter.Quoted)
  val union = UnionTypescriptRenderer[Key, Id, Typescript.Value](renderer = this).map(Typescript.Value.apply)

  override def render[B](schema: Key[B]): Typescript.Value = schema match
    case Key.Primitive.String(self) => ??? // PrimitiveTypescriptRenderer.render(self)
    case Key.Constant(self)         => ??? // constant.render(schema = self)
    case Key.Enumeration(self)      => ??? // enumeration.render(schema = self)
    case Key.Union(self)            => union.render(schema = self)
