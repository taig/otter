package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*
import io.taig.otter.ZodConst
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState
import io.taig.otter.schema.Schema

import scala.collection.immutable.ListMap

final class NamespaceZodRenderer[S[_]: Schema](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[ZodExpression]]:
  val expression = ZodRenderer[S](renderer)

  override def render[A](schema: S[A]): ZodState[ZodExpression] = State: state =>
    schema.metadata(name) match
      case Some(name) =>
        val reference = ZodConst(namespace = schema.metadata(namespace), name)
        val (update, result) = expression.render(schema).run(initial = state).value
        (update.updatedWith(reference)(_ => Some(result)), ZodExpression.Referenced(reference, result))
      case None => expression.render(schema).run(initial = state).value.map(ZodExpression.Inline.apply)
