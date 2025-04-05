package io.taig.otter

import cats.syntax.all.*
import cats.data.State
import io.taig.otter.Json.Key

object JsonKeyZodRenderer extends Renderer[Json.Key, ZodState[Expression]]:
  override def apply[A](codec: Json.Key[A]): ZodState[Expression] =
    NamespaceZodRenderer(renderer = Raw)(codec)

  object Raw extends Renderer[Json.Key, ZodState[String]]:
    val constants = ReferenceConstantZodEncoder(encoder = JsonPrimitiveStringEncoder)

    override def apply[A](codec: Json.Key[A]): ZodState[String] = codec.value match
      case codec: Constant[Json.Key, ?] =>
        constants(codec.codec)
        ??? // State.pure(s"z.literal(${constants(codec.codec)})")
      // case codec: Primitive.String[?]         => State.pure(PrimitiveZodRenderer(codec))
      case codec: Union.Untagged[Json.Key, ?] =>
        codec.branches
          .traverse((_, codec) => JsonKeyZodRenderer(codec.value))
          .map: expressions =>
            s"""z.union([
               |${indent(expressions.map(_.show).mkString_(",\n"))}
               |])""".stripMargin
