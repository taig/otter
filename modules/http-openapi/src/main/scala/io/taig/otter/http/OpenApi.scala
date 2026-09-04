package io.taig.otter.http

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.Metadata

/** The vocabulary an OpenAPI document is built out of.
  *
  * A document is an `io.circe.Json`, for the reason [[JsonSchema]] gives for the same choice: an OpenAPI document *is*
  * a JSON document, a closed model of one would have to be reopened by the next version and by every `x-` extension a
  * vendor adds, and what a closed model would have bought -- that an ill formed document cannot be built -- is bought
  * back by making every keyword go through a constructor here.
  *
  * [[JsonSchema.merge]] and the rest are reused rather than restated. A merge that holds key positions is what keeps a
  * golden document stable when a description is added to an operation that already had one, and that is as true here as
  * it is one tier down.
  */
object OpenApi:
  /** The [[Metadata.Namespace]] the OpenAPI renderer reads its attributes from. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("openapi")

  /** Five layers, most specific first: what OpenAPI says wins over what HTTP says, which wins over what JSON Schema
    * says, which wins over what JSON says, which wins over what is said globally.
    *
    * [[Http.Namespace]] is in the chain because this renderer reads attributes that are HTTP's rather than any document
    * format's -- a [[io.taig.otter.http.HttpKeys.filename]] on a part is not something a JSON Schema renderer would
    * ever look for.
    */
  val Namespaces: NonEmptyList[Metadata.Namespace] = NonEmptyList.of(
    OpenApi.Namespace,
    Http.Namespace,
    JsonSchema.Namespace,
    Json.Namespace,
    Metadata.Namespace.Global
  )

  /** Where a shared schema is declared, and therefore what a `$ref` points at.
    *
    * Spelled as the pointer path rather than a single keyword so that [[JsonSchema.ref]] produces
    * `#/components/schemas/Name` unchanged. The renderer nests the definitions this names under `components` on the way
    * out, since a document has no top level key by that name.
    */
  val Definitions: String = "components/schemas"

  /** The version this renderer writes. */
  val Version: String = "3.1.0"

  /** What a document says about itself. Required by the specification, so it is a parameter and not an attribute. */
  final case class Info(title: String, version: String, description: Option[String] = None)

  def obj(fields: (String, CirceJson)*): CirceJson = CirceJson.obj(fields*)

  /** A parameter object, which is the one place OpenAPI names a position rather than a shape. */
  def parameter(name: String, in: String, required: Boolean, schema: CirceJson): CirceJson =
    OpenApi.obj(
      "name" -> CirceJson.fromString(name),
      "in" -> CirceJson.fromString(in),
      "required" -> CirceJson.fromBoolean(required),
      "schema" -> schema
    )

  /** A media type object: what a body of this type looks like. */
  def content(entries: List[(String, CirceJson)]): CirceJson =
    CirceJson.obj(entries.map((media, schema) => media -> OpenApi.obj("schema" -> schema))*)

  val InPath: String = "path"

  val InQuery: String = "query"

  val InHeader: String = "header"
