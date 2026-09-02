package io.taig.otter.codec

import cats.data.NonEmptyList
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

/** [[JsonTypescriptExpressionOverrideRenderer]] for the type sort. */
final class JsonTypescriptTypeOverrideRenderer(
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[Json.Node, Typescript.Type]
) extends Renderer[Json.Node, Typescript.Type]:
  override def render[W, R](json: Json.Node[W, R]): Typescript.Type =
    Json
      .attr(namespaces, Json.metadata(json), TypescriptKeys.tpe)
      .getOrElse(renderer.render(json))
