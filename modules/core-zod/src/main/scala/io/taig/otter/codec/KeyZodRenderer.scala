package io.taig.otter.codec

import cats.Id
import io.taig.otter.Key
import io.taig.otter.Zod

object KeyZodRenderer extends Renderer[Key, Zod]:
  val constant = ConstantZodRenderer[Key, Id](printer = KeyPrinter.Quoted)
  val enumeration = EnumerationZodRenderer(printer = KeyPrinter.Quoted)
  val union = UnionZodRenderer[Key, Id](renderer = this)

  override def render[A](schema: Key[A]): Zod = schema match
    case Key.Primitive.String(self) => PrimitiveZodRenderer.render(schema = self)
    case Key.Constant(self)         => constant.render(schema = self)
    case Key.Enumeration(self)      => enumeration.render(schema = self)
    case Key.Union(self)            => union.render(schema = self)
