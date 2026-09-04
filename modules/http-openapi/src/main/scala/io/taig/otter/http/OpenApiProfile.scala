package io.taig.otter.http

import io.taig.otter.JsonSchemaProfile

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
  * Written out rather than as a `copy` of [[io.taig.otter.JsonSchemaProfile.Draft202012]], for the reason `Strict` is:
  * which decisions are independent should be visible rather than inferred from a diff.
  */
object OpenApiProfile:
  val V31: JsonSchemaProfile = JsonSchemaProfile(
    dialect = None,
    definitions = Some(OpenApi.Definitions),
    recursion = true,
    additionalProperties = None,
    total = false,
    prefixItems = true,
    dictionaries = true,
    coercion = true,
    branchTitles = true,
    nullability = JsonSchemaProfile.Nullability.AnyOf,
    constraints = true,
    formats = None
  )
