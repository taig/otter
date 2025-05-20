package io.taig.otter.codec

import io.taig.otter.Key
import io.taig.otter.Key.Constant
import io.taig.otter.Key.Primitive
import io.taig.otter.Key.Union
import cats.Id

object KeyZodRenderer extends Renderer[Key, String]:
  val constant = ConstantZodRenderer[Key, Id](printer = KeyPrinter.Quoted)
  val enumeration = EnumerationZodRenderer(printer = KeyPrinter.Quoted)
  val union = UnionZodRenderer[Key, Id, String](renderer = this)

  override def render[A](schema: Key[A]): String = schema match
    case Key.Constant(self)    => constant.render(schema = self)
    case Key.Enumeration(self) => enumeration.render(schema = self)
    case Key.Primitive(self)   => PrimitiveZodRenderer.render(schema = self)
    case Key.Union(self)       => union.render(schema = self)
