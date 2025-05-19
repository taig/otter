package io.taig.otter

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*
import io.taig.otter.schema.Schema

import scala.collection.immutable.ListMap

final class NamespaceZodRenderer[S[_]: Schema](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[Expression]]:
  val force = ZodRenderer[S](renderer)

  override def render[A](schema: S[A]): ZodState[Expression] = State: state =>
    schema.metadata(name) match
      case Some(name) =>
        val reference = Const(namespace = schema.metadata(namespace), name)
        val (update, result) = force.render(schema).run(initial = state).value
        (update.updatedWith(reference)(_ => Some(result)), Expression.Referenced(reference, result))
      case None => force.render(schema).run(initial = state).value.map(Expression.Inline.apply)
