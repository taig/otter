package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.ZodState
import io.taig.otter.Zod
import io.taig.otter.ZodKeys
import io.taig.otter.Keys
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.toSymbol

final class ZodRenderer[S[_]: SchemaInvariant](
    renderer: Renderer[S, ZodState[Zod]]
) extends Renderer[S, ZodState[Zod]]:
  override def render[A](schema: S[A]): ZodState[Zod] = schema.metadata(Keys.name).map(toSymbol) match
    case Some(name) =>
      State: state =>
        if state.stack.contains_(name)
        then (state, Zod.Reference(name))
        else
          state.references.get(name) match
            case Some(value) => (state, Zod.Reference(name))
            case None =>
              val (update, typescript) = renderMetadataOrSchema(schema).run(initial = state.push(name)).value

              (
                update.modifyReferences(_.updatedWith(name)(_ => typescript.some)).pop(name),
                Zod.Reference(name)
              )
    case None => renderMetadataOrSchema(schema)

  def renderMetadataOrSchema[A](schema: S[A]): ZodState[Zod] = schema.metadata(ZodKeys.zod) match
    case Some(zod) => State.pure(zod)
    case None      => renderer.render(schema)
