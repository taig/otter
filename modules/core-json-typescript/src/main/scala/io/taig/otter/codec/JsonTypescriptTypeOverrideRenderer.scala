package io.taig.otter.codec

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

final class JsonTypescriptTypeOverrideRenderer[F[_]: Applicative](
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Type]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Type]]:
  override def render[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Type] = json
    .match
      case json: Json.Read[?]  => json.attr(namespaces, TypescriptKeys.tpe)
      case json: Json.Write[?] => json.attr(namespaces, TypescriptKeys.tpe)
    .fold(renderer.render(json))(_.pure)
