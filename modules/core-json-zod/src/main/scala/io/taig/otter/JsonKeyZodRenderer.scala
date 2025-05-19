package io.taig.otter

import cats.data.State
import io.taig.otter.codec.PrimitivePrinter

object JsonKeyZodRenderer extends Renderer[Key, ZodState[Expression]]:
  override def apply[A](schema: Key[A]): ZodState[Expression] =
    NamespaceZodRenderer(renderer = Raw).render(schema)

  object Raw extends Renderer[Key, ZodState[String]]:
    override def apply[A](schema: Key[A]): ZodState[String] = schema match
      case Key.Constant(self)    => State.pure(apply(schema = self))
      case Key.Enumeration(self) => State.pure(apply(schema = self))
      case Key.Primitive(self)   => State.pure(PrimitiveZodRenderer.render(schema = self))
      // case Key.Union(self) =>
      //   UnionZodRenderer[Key](render = [A] => (_: String, schema: Key[A]) => JsonKeyZodRenderer(schema))(self)

    def apply(schema: Constant[Key.Primitive, ?]): String =
      s"z.literal(${apply(reference = schema.schema)})"

    def apply(schema: Enumeration[Key.Primitive, ?]): String =
      EnumerationZodRenderer(printer = JsonKeyPrimitivePrinter)(schema)

    def apply[A](reference: Reference.Constant[Key.Primitive, A]): String =
      PrimitivePrinter.Quoted.encode(schema = reference.self.value.self, reference.value)
