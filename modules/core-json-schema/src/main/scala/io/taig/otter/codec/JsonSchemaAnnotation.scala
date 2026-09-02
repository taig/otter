package io.taig.otter.codec

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys
import io.taig.otter.Metadata

/** What a schema's metadata says about a document, beyond what the node it annotates derives.
  *
  * [[Keys.title]] and [[Keys.description]] have been declared since before there was anything to read them; this is the
  * first interpreter that does. They lead, because that is where a reader looks for them, and the rest trails the
  * structure it annotates.
  *
  * A field carries an annotation of its own beside the one on what it holds, which is why this takes a [[Metadata]]
  * rather than a node: `field("pages", int).attr(Keys.description, ...)` describes the property and
  * `int.attr(Keys.description, ...)` describes the type, and the two are applied at different points.
  */
object JsonSchemaAnnotation:
  def apply(
      namespaces: NonEmptyList[Metadata.Namespace],
      metadata: Metadata,
      schema: CirceJson
  ): CirceJson =
    def attr[A](key: Metadata.Key[A]): Option[A] = Json.attr(namespaces, metadata, key)

    val leading = List(
      attr(Keys.title).map(value => "title" -> CirceJson.fromString(value)),
      attr(Keys.description).map(value => "description" -> CirceJson.fromString(value))
    ).flatten

    val trailing = List(
      attr(JsonSchemaKeys.default).map(value => "default" -> value),
      attr(JsonSchemaKeys.examples).map(values => "examples" -> CirceJson.fromValues(values)),
      attr(JsonSchemaKeys.deprecated).map(value => "deprecated" -> CirceJson.fromBoolean(value))
    ).flatten

    /* The first merge is what puts a label ahead of the structure it labels; the second is what makes it win, because
     * a merge takes the right side's value and the structure may already carry one of its own. */
    val labelled = JsonSchema.merge(JsonSchema.merge(CirceJson.obj(leading*), schema), leading*)
    val annotated = JsonSchema.merge(labelled, trailing*)

    attr(JsonSchemaKeys.keywords).fold(annotated)(JsonSchema.merge(annotated, _))
