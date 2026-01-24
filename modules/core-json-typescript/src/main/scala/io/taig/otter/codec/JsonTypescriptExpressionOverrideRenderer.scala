package io.taig.otter.codec

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

final class JsonTypescriptExpressionOverrideRenderer[F[_]: Applicative](
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]:
  override def render[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Expression] = json
    .match
      case json: Json.Read[?]  => json.attr(namespaces, TypescriptKeys.expression)
      case json: Json.Write[?] => json.attr(namespaces, TypescriptKeys.expression)
    .fold(renderer.render(json))(_.pure)
