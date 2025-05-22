package io.taig.otter.codec

import cats.Id
import io.taig.otter.Key
import io.taig.otter.Typescript

object KeyTypescriptRenderer extends Renderer[Key, Typescript]:
  val constant = ConstantTypescriptRenderer[Key, Id](printer = KeyPrinter.Quoted)
  val enumeration = EnumerationTypescriptRenderer(printer = KeyPrinter.Quoted)
  val union = UnionTypescriptRenderer[Key, Id](renderer = this)

  override def render[A](schema: Key[A]): Typescript = schema match
    case Key.Constant(self)    => constant.render(schema = self)
    case Key.Enumeration(self) => enumeration.render(schema = self)
    case Key.Primitive(self)   => PrimitiveTypescriptRenderer.render(schema = self)
    case Key.Union(self)       => union.render(schema = self)
