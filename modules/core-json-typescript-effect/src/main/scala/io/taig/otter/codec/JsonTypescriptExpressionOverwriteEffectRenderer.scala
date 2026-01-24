package io.taig.otter.codec

import cats.Applicative
import io.taig.otter.Typescript
import io.taig.otter.Json
import io.taig.otter.TypescriptEffect
import io.taig.otter.Keys
import cats.syntax.all.*
import io.taig.otter.JsonTypescriptEffect

final class JsonTypescriptExpressionEffectOverwriteRenderer[F[_]: Applicative](
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]:
  override def render[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Expression] = json
    .match
      case json: Json.Read[?]  => json.attr(JsonTypescriptEffect.Namespace, TypescriptEffect.Namespace)(Keys.overwrite)
      case json: Json.Write[?] => json.attr(JsonTypescriptEffect.Namespace, TypescriptEffect.Namespace)(Keys.overwrite)
    .map(Typescript.Expression.Eval.apply)
    .fold(renderer.render(json))(_.pure)
