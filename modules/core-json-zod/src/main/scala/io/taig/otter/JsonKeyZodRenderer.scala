package io.taig.otter
import cats.data.State

object JsonKeyZodRenderer extends Renderer[Json.Key, ZodState[Expression]]:
  override def apply[A](codec: Json.Key[A]): ZodState[Expression] =
    NamespaceZodRenderer(renderer = Raw)(codec)

  object Raw extends Renderer[Json.Key, ZodState[String]]:
    override def apply[A](codec: Json.Key[A]): ZodState[String] = codec match
      case Json.Key.Constant(self)    => State.pure(apply(codec = self))
      case Json.Key.Enumeration(self) => State.pure(apply(codec = self))
      case Json.Key.Primitive(self)   => State.pure(PrimitiveZodRenderer(codec = self))
      // case Json.Key.Union(self) =>
      //   UnionZodRenderer[Json.Key](render = [A] => (_: String, codec: Json.Key[A]) => JsonKeyZodRenderer(codec))(self)

    def apply(codec: Constant[Json.Key.Primitive, ?]): String =
      s"z.literal(${apply(reference = codec.codec)})"

    def apply(codec: Enumeration[Json.Key.Primitive, ?]): String =
      EnumerationZodRenderer(printer = JsonKeyPrimitivePrinter)(codec)

    def apply[A](reference: Reference.Constant[Json.Key.Primitive, A]): String =
      PrimitivePrinter.Quoted(codec = reference.self.value.self, reference.value)
