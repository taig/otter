package io.taig.otter.codec

import cats.Applicative
import cats.Show
import cats.syntax.all.*
import io.taig.otter.Union
import io.taig.otter.indent

final class UnionZodRenderer[S[_], T[_]: Applicative, A: Show](renderer: Renderer[S, T[A]])
    extends Renderer[Union[S, *], T[String]]:
  override def render[A](schema: Union[S, A]): T[String] = schema.schemas
    .traverse(reference => renderer.render(schema = reference.value))
    .map: values =>
      s"""z.union([
         |${indent(values.map(_.show).mkString_(",\n"))}
         |])""".stripMargin
