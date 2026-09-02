package io.taig.otter.codec

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

/** Hands back what a schema says it renders as, if it says anything, and otherwise renders it. */
final class JsonTypescriptExpressionOverrideRenderer[F[_]: Applicative](
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[Json.Node, F[Typescript.Expression]]
) extends Renderer[Json.Node, F[Typescript.Expression]]:
  override def render[W, R](json: Json.Node[W, R]): F[Typescript.Expression] =
    Json
      .attr(namespaces, Json.metadata(json), TypescriptKeys.expression)
      .fold(renderer.render(json))(_.pure)
