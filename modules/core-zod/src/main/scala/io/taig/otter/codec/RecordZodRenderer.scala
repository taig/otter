package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState
import io.taig.otter.indent

final class RecordZodRenderer[S[_]](renderer: Renderer[S, ZodState[(String, ZodExpression)]])
    extends Renderer[Record[S, *], ZodState[String]]:
  override def render[A](schema: Record[S, A]): ZodState[String] =
    schema.fields
      .traverse(field => renderer.render(schema = field.value))
      .map:
        case Chain.nil           => "z.object({})"
        case Chain((key, value)) => show"z.object({ $key: $value })"
        case values =>
          val fields = values.map((key, value) => show"$key: $value").mkString_(",\n")
          s"""z.object({
             |${indent(fields)}
             |})""".stripMargin
