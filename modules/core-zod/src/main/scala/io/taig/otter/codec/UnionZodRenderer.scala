package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Union
import io.taig.otter.indent

final class UnionZodRenderer[S[_]](renderer: Renderer[S, String]) extends Renderer[Union[S, *], String]:
  override def render[A](schema: Union[S, A]): String =
    val values = schema.schemas
      .map(reference => renderer.render(schema = reference.value))
      .mkString_(",\n")

    s"""z.union([
       |${indent(values)}
       |])""".stripMargin
