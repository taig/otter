package io.taig.otter.http

import io.circe.Json as CirceJson
import io.taig.otter.Constraint
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.codec.ConstraintJsonSchema

/** Which JSON Schema an OpenAPI document's schemas are written in.
  *
  * A value rather than a module, which is the rule `core-json-schema` sets and this is the third instance of: what
  * varies between draft 2020-12, a strict structured output consumer and OpenAPI is a consumer's profile of one
  * dialect, not a target library.
  *
  * OpenAPI 3.1 aligned itself with draft 2020-12, so almost every answer here is that draft's. The two that differ are
  * where a shared schema lives -- `components/schemas` rather than `$defs` -- and that a schema inside a document
  * declares no dialect of its own, because the document declares it once for all of them.
  *
  * Written out rather than as an override of [[JsonSchemaProfile.Draft202012]], for the reason `Strict` is: which
  * decisions are independent should be visible rather than inferred from a diff.
  */
object OpenApiProfile:
  val V31: JsonSchemaProfile = new JsonSchemaProfile:
    override val dialect: Option[String] = None
    override val definitions: Option[String] = Some(OpenApi.Definitions)
    override val recursion: Boolean = true
    override val additionalProperties: Option[Boolean] = None
    override val total: Boolean = false
    override val prefixItems: Boolean = true
    override val dictionaries: Boolean = true
    override val coercion: Boolean = true
    override val branchTitles: Boolean = true
    override def nullable(schema: CirceJson): CirceJson = JsonSchemaProfile.Nullability.AnyOf(schema)
    override def keyword(constraint: Constraint): Option[(String, CirceJson)] = ConstraintJsonSchema.keyword(constraint)
    override def format(name: String): Option[String] = Some(name)
