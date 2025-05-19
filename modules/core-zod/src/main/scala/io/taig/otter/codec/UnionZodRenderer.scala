package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import io.taig.otter.Union
import io.taig.otter.indent

final class UnionZodRenderer[S[_]](renderer: Renderer[S, ZodState[ZodExpression]])
    extends Renderer[Union[S, *], ZodState[String]]:
  override def render[A](schema: Union[S, A]): ZodState[String] = schema.schemas
    .traverse(reference => renderer.render(schema = reference.value))
    .map: values =>
      s"""z.union([
         |${indent(values.map(value => show"$value").mkString_(",\n"))}
         |])""".stripMargin
