package io.taig.otter.http.codec

import cats.data.State
import io.taig.otter.Json
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.codec.JsonStateTypescriptRenderer
import io.taig.otter.codec.JsonTypescriptContext
import io.taig.otter.codec.JsonTypescriptExpressionEffectRenderer
import io.taig.otter.http.HttpTypescriptEffect

import scala.compiletime.asMatchable

/** JSON payloads, as the `effect` `Schema` values that read and write them.
  *
  * The whole of what this module adds to [[TypescriptPayload]], and it adds nothing to the generator that made the
  * schema: [[JsonStateTypescriptRenderer]] is the fixpoint every TypeScript target shares and
  * [[JsonTypescriptEffect.Target]] is the three things effect spells its own way, both reused as they stand. What is
  * chosen here is only the namespace chain, so that an endpoint's attributes are read before a payload's.
  */
object TypescriptEffectPayload:
  val json: TypescriptPayload = new TypescriptPayload:
    override def render(side: Side, payload: Any): Option[State[JsonTypescriptContext, Typescript.Expression]] =
      payload.asMatchable match
        case json: Json.Schema[?, ?, ?] @unchecked => Some(renderer(side).render[Nothing, Any](json))
        case _                                     => None

    override def name(payload: Any): Option[String] = payload.asMatchable match
      case json: Json.Schema[?, ?, ?] @unchecked => Json.name(HttpTypescriptEffect.Namespaces, json)
      case _                                     => None

  private def renderer(side: Side): JsonStateTypescriptRenderer = new JsonStateTypescriptRenderer(
    side,
    HttpTypescriptEffect.Namespaces,
    JsonTypescriptEffect.Target,
    identity,
    renderer => new JsonTypescriptExpressionEffectRenderer(side, renderer)
  )
