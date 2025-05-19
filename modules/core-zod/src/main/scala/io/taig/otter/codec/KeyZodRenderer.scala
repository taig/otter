package io.taig.otter.codec

import io.taig.otter.Key
import io.taig.otter.Key.Constant
import io.taig.otter.Key.Primitive
import io.taig.otter.Key.Union
import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import cats.data.State
import cats.syntax.all.*

object KeyZodRenderer extends Renderer[Key, ZodState[ZodExpression]]:
  val renderer = NamespaceZodRenderer(renderer = ZodRenderer(renderer = Expression))

  val constant = ConstantZodRenderer(printer = KeyPrinter.Quoted.map(_.some), renderer = Expression)
  val enumeration = EnumerationZodRenderer(printer = KeyPrinter.Quoted)
  val union = UnionZodRenderer(renderer = this)

  override def render[A](schema: Key[A]): ZodState[ZodExpression] = renderer.render(schema)

  object Expression extends Renderer[Key, ZodState[String]]:
    override def render[A](schema: Key[A]): ZodState[String] = schema match
      case Key.Constant(self)    => constant.render(schema = self)
      case Key.Enumeration(self) => State.pure(enumeration.render(schema = self))
      case Key.Primitive(self)   => State.pure(PrimitiveZodRenderer.render(schema = self))
      case Key.Union(self)       => union.render(schema = self)
