package io.taig.otter.codec

import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import io.taig.otter.Tuple
import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.indent

final class TupleZodRenderer[S[_]](renderer: Renderer[S, ZodState[ZodExpression]])
    extends Renderer[Tuple[S, *], ZodState[String]]:
  override def render[A](schema: Tuple[S, A]): ZodState[String] =
    schema.schemas
      .traverse(schema => renderer.render(schema = schema.value))
      .map:
        case Chain.nil         => "z.tuple([])"
        case Chain(expression) => show"z.tuple([$expression])"
        case expressions =>
          show"""z.tuple([
                |${indent(expressions.mkString_(",\n"))}
                |])""".stripMargin
