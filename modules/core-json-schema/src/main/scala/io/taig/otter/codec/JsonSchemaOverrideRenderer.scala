package io.taig.otter.codec

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Metadata

/** Lets a schema say what it renders as, instead of being asked.
  *
  * [[JsonSchemaKeys.schema]] replaces a node's document wholesale, which is the escape hatch for everything a renderer
  * derives wrongly or cannot derive at all -- a `format` whose two nodes are indistinguishable, a dialect keyword with
  * no counterpart here. The annotations are applied either way, so an override still carries its own title.
  */
final class JsonSchemaOverrideRenderer[F[_]: Applicative](
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[Json.Node, F[CirceJson]]
) extends Renderer[Json.Node, F[CirceJson]]:
  override def render[W, R](json: Json.Node[W, R]): F[CirceJson] =
    val metadata = Json.metadata(json)

    Json
      .attr(namespaces, metadata, JsonSchemaKeys.schema)
      .fold(renderer.render(json))(_.pure)
      .map(JsonSchemaAnnotation(namespaces, metadata, _))
