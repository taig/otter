package io.taig.otter.codec

import io.taig.otter.Key
import io.taig.otter.ZodState
import io.taig.otter.Key.Constant
import io.taig.otter.Key.Primitive
import io.taig.otter.Key.Union
import cats.data.State

object KeyZodRenderer extends Renderer[Key, String]:
  override def render[A](schema: Key[A]): String = schema match
    case Key.Constant(self)    => ???
    case Key.Enumeration(self) => EnumerationZodRenderer(printer = KeyPrinter.Quoted).render(schema = self)
    case Key.Primitive(self)   => PrimitiveZodRenderer.render(schema = self)
    case Key.Union(self)       => ???
