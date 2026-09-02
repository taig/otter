package io.taig.otter

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.circe.JsonObject

/** The vocabulary a JSON Schema document is built out of.
  *
  * A document is an `io.circe.Json`, because a JSON Schema is a JSON document and inventing a closed model of one would
  * only have to be reopened by the next dialect: OpenAPI adds `discriminator`, a vendor adds `x-`, and a sealed type
  * answers either with a new case or with a raw escape hatch that admits defeat.
  *
  * What a closed model would have bought is that an ill formed document cannot be built, and these constructors are how
  * that is bought back instead. No call site writes a keyword string; every one of them goes through a member here, so
  * the keywords a renderer can emit are the ones named in this file.
  */
object JsonSchema:
  /** The [[Metadata.Namespace]] the JSON Schema renderers read their attributes from.
    *
    * An attribute set here wins over the same attribute set for JSON generally, which wins over one set globally.
    */
  val Namespace: Metadata.Namespace = Metadata.Namespace("json-schema")

  /** Three layers, because there is no target library below JSON Schema the way there is below TypeScript. A profile
    * that wants its own overrides prepends a namespace of its own rather than adding a module.
    */
  val Namespaces: NonEmptyList[Metadata.Namespace] =
    NonEmptyList.of(JsonSchema.Namespace, Json.Namespace, Metadata.Namespace.Global)

  /** The schema that says nothing, which every document is. */
  val Anything: CirceJson = CirceJson.obj()

  /** The schema nothing satisfies. */
  val Nothing: CirceJson = CirceJson.False

  def obj(fields: (String, CirceJson)*): CirceJson = CirceJson.obj(fields*)

  def typed(name: String): CirceJson = JsonSchema.obj("type" -> CirceJson.fromString(name))

  /** `right` over `left`, a key at a time.
    *
    * A key `left` already has keeps the position it has and takes `right`'s value; a key only `right` has is appended.
    * Holding the position is what makes a golden document stable when an annotation is added to a schema that already
    * carried one.
    */
  def merge(left: CirceJson, right: CirceJson): CirceJson = (left.asObject, right.asObject) match
    case (Some(left), Some(right)) =>
      val updated = left.toList.map((key, value) => key -> right(key).getOrElse(value))
      val added = right.toList.filterNot((key, _) => left.contains(key))
      CirceJson.fromJsonObject(JsonObject.fromIterable(updated ++ added))
    case _ => right

  def merge(left: CirceJson, right: (String, CirceJson)*): CirceJson =
    if right.isEmpty then left else JsonSchema.merge(left, CirceJson.obj(right*))

  /** A reference to a definition, as the JSON Pointer that names it.
    *
    * `~` and `/` are the two characters a pointer segment cannot hold, and [[Keys.name]] is free form, so both are
    * escaped rather than trusted.
    */
  def ref(definitions: String, name: String): CirceJson =
    val escaped = name.replace("~", "~0").replace("/", "~1")
    JsonSchema.obj("$ref" -> CirceJson.fromString(s"#/$definitions/$escaped"))

  /** The alternation of `schemas`, flattened when it is handed one.
    *
    * A single alternative is that alternative: wrapping it would say the same thing at the cost of a level, and a
    * `nullable` applied to the result would then have to look through the wrapper to find it.
    */
  def anyOf(schemas: NonEmptyList[CirceJson]): CirceJson = schemas match
    case NonEmptyList(schema, Nil) => schema
    case schemas                   => JsonSchema.obj("anyOf" -> CirceJson.fromValues(schemas.toList))

  /** The alternatives a schema is made of, or the schema itself where it is not an alternation. */
  def alternatives(schema: CirceJson): NonEmptyList[CirceJson] =
    schema.asObject
      .flatMap(_("anyOf"))
      .flatMap(_.asArray)
      .flatMap(values => NonEmptyList.fromList(values.toList))
      .getOrElse(NonEmptyList.one(schema))

  val Null: CirceJson = JsonSchema.typed("null")
