package io.taig.otter.syntax

import io.circe.Encoder
import io.circe.Json as CirceJson
import io.taig.otter.Annotated
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.Keys

/** What a schema says about itself that a renderer cannot derive.
  *
  * Written against anything that carries metadata rather than against a node, because every one of these is as true of
  * a record as of the field that holds it -- and where both carry one, the field's is the more specific and wins.
  *
  * Everything here is set in the JSON Schema namespace, so it is heard by this renderer and by nothing else. The
  * general form, `.attr(Keys.description, ...)`, says it to every interpreter at once.
  */
trait JsonSchemaSyntax:
  extension [T](fa: T)(using T: Annotated[T])
    /** A short label. */
    def title(value: String): T = fa.attr(JsonSchema.Namespace, Keys.title, value)

    def description(value: String): T = fa.attr(JsonSchema.Namespace, Keys.description, value)

    /** The value to assume where one is absent.
      *
      * Says it as a document, because a renderer sees a node with its two sides unrelated and cannot push a value back
      * through it. Nothing checks that this agrees with the default the schema actually applies.
      */
    def default[A: Encoder](value: A): T =
      fa.attr(JsonSchema.Namespace, JsonSchemaKeys.default, Encoder[A].apply(value))

    def examples[A: Encoder](values: A*): T =
      fa.attr(JsonSchema.Namespace, JsonSchemaKeys.examples, values.toList.map(Encoder[A].apply))

    def deprecated: T = fa.attr(JsonSchema.Namespace, JsonSchemaKeys.deprecated, true)

    /** Keywords merged over what the node rendered as. */
    def keywords(value: CirceJson): T = fa.attr(JsonSchema.Namespace, JsonSchemaKeys.keywords, value)

    /** The document the node renders as, instead of the one derived from it. */
    def schema(value: CirceJson): T = fa.attr(JsonSchema.Namespace, JsonSchemaKeys.schema, value)

object JsonSchemaSyntax extends JsonSchemaSyntax
