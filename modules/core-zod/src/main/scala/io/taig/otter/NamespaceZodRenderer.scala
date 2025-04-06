package io.taig.otter

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*

import scala.collection.immutable.ListMap

final class NamespaceZodRenderer[S[_]: CodecInvariant](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[Expression]]:
  val force = ZodRenderer(renderer)

  override def apply[A](codec: S[A]): ZodState[Expression] = State: state =>
    codec.metadata.get(name) match
      case Some(name) =>
        val reference = Const(namespace = codec.metadata.get(namespace), name)
        val (update, result) = force(codec).run(initial = state).value
        (update.updatedWith(reference)(_ => Some(result)), Expression.Referenced(reference, result))
      case None => force(codec).run(initial = state).value.map(Expression.Inline.apply)

object NamespaceZodRenderer:
  def apply[S[_]: CodecInvariant](renderer: Renderer[S, ZodState[String]]): Renderer[S, ZodState[Expression]] =
    new NamespaceZodRenderer(renderer)
