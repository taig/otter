package io.taig.otter.openapi.circe

import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.*
import io.taig.otter.circe.syntax.*
import io.taig.otter.circe.instance.given

object instance:
  inline given [A <: Matchable: Encoder.AsObject]: Encoder.AsObject[A | Reference] =
    case reference: Reference => reference.asJsonObject
    case a: A => a.asJsonObject

  given Encoder.AsObject[Components] = components =>
    JsonObject(
      "schemas" := components.schemas.asJsonObject.toNonEmpty,
      "responses" := components.responses.asJsonObject.toNonEmpty,
      "parameters" := components.parameters.asJsonObject.toNonEmpty,
      "examples" := components.examples.asJsonObject.toNonEmpty,
      "requestBodies" := components.requestBodies.asJsonObject.toNonEmpty,
      "headers" := components.headers.asJsonObject.toNonEmpty,
      "securitySchemes" := components.securitySchemes.asJsonObject.toNonEmpty,
      "links" := components.links.asJsonObject.toNonEmpty,
      "callbacks" := components.callbacks.asJsonObject.toNonEmpty,
      "pathItems" := components.pathItems.asJsonObject.toNonEmpty
    ).dropNullValues

  given Encoder.AsObject[Contact] = contact =>
    JsonObject(
      "name" := contact.name,
      "url" := contact.url,
      "email" := contact.email
    ).dropNullValues

  given [A: Encoder.AsObject]: Encoder.AsObject[Extended[A]] = extended =>
    extended.value.asJsonObject ++ extended.extensions.asJsonObject

  given Encoder.AsObject[Extensions] = _.toChain.asJsonObject

  given Encoder.AsObject[ExternalDocumentation] = documentation =>
    JsonObject(
      "description" := documentation.description,
      "url" := documentation.url
    ).dropNullValues

  given Encoder.AsObject[Header] = header =>
    JsonObject(
      "name" := header.name,
      "description" := header.description,
      "externalDocs" := header.externalDocs
    )

  given Encoder.AsObject[Info] = info =>
    JsonObject(
      "title" := info.title,
      "version" := info.version,
      "summary" := info.summary,
      "description" := info.description,
      "termsOfService" := info.termsOfService,
      "contact" := info.contact,
      "license" := info.license
    ).dropNullValues

  given Encoder.AsObject[License] = license =>
    JsonObject(
      "name" := license.name,
      "identifier" := license.identifier,
      "url" := license.url
    ).dropNullValues

  given Encoder.AsObject[MediaType] = mediaType => JsonObject("schema" := mediaType.schema).dropNullValues

  given Encoder.AsObject[OpenApi] = openapi =>
    JsonObject(
      "openapi" := openapi.openapi,
      "info" := openapi.info,
      "jsonSchemaDialect" := openapi.jsonSchemaDialect,
      "servers" := Some(openapi.servers).filter(_.nonEmpty),
      "paths" := openapi.paths,
      "webhooks" := Some(openapi.webhooks).filter(_.nonEmpty),
      "components" := openapi.components,
      "security" := openapi.security,
      "tags" := Some(openapi.tags).filter(_.nonEmpty),
      "externalDocs" := openapi.externalDocs
    ).dropNullValues

  given Encoder.AsObject[Operation] = operation =>
    JsonObject(
      "tags" := operation.tags,
      "summary" := operation.summary,
      "description" := operation.description,
      "externalDocs" := operation.externalDocs,
      "operationId" := operation.operationId,
      "parameters" := Some(operation.parameters).filter(_.nonEmpty),
      "requestBody" := operation.requestBody,
      "responses" := Some(operation.responses.asJsonObject).filter(_.nonEmpty),
      "callbacks" := Some(operation.callbacks).filter(_.nonEmpty),
      "deprecated" := operation.deprecated,
      "security" := operation.security,
      "servers" := Some(operation.servers).filter(_.nonEmpty)
    ).dropNullValues

  given Encoder.AsObject[PathItem] = item =>
    JsonObject(
      "$ref" := item.ref,
      "summary" := item.summary,
      "description" := item.description,
      "get" := item.get,
      "put" := item.put,
      "post" := item.post,
      "delete" := item.delete,
      "options" := item.options,
      "head" := item.head,
      "patch" := item.patch,
      "trace" := item.trace,
      "servers" := Some(item.servers).filter(_.nonEmpty),
      "parameters" := Some(item.parameters).filter(_.nonEmpty)
    ).dropNullValues

  given Encoder.AsObject[Paths] = _.toChain.asJsonObject

  given Encoder.AsObject[Parameter] = parameter =>
    JsonObject(
      "in" := parameter.in,
      "name" := parameter.name,
      "description" := parameter.description,
      "required" := parameter.required,
      "deprecated" := parameter.deprecated,
      "schema" := parameter.schema
    ).dropNullValues

  given Encoder.AsObject[Reference] = reference =>
    JsonObject(
      "$ref" := reference.ref,
      "summary" := reference.summary,
      "description" := reference.description
    ).dropNullValues

  given Encoder.AsObject[RequestBody] = body =>
    JsonObject(
      "content" := body.content.asJsonObject.toNonEmpty,
      "description" := body.description,
      "required" := body.required
    ).dropNullValues

  given Encoder.AsObject[Response] = response =>
    JsonObject(
      "description" := response.description,
      "headers" := response.headers.asJsonObject.toNonEmpty,
      "content" := response.content.asJsonObject.toNonEmpty,
      "links" := response.links.asJsonObject.toNonEmpty
    ).dropNullValues

  given Encoder.AsObject[Responses] = responses =>
    responses.values.asJsonObject ++ JsonObject("default" := responses.default).dropNullValues

  given Encoder.AsObject[Schema] =
    case codec: Schema.Array =>
      JsonObject(
        "type" := "array",
        "format" := codec.format,
        "description" := codec.description,
        "items" := codec.items
      ).dropNullValues
    case codec: Schema.Enumeration => JsonObject("type" := codec.tpe, "enum" := codec.enums.map(_.asJson))
    case codec: Schema.OneOf       => JsonObject("oneOf" := codec.codecs)
    case codec: Schema.Object =>
      JsonObject(
        "type" := "object",
        "format" := codec.format,
        "description" := codec.description,
        "properties" := codec.properties.asJsonObject.toNonEmpty,
        "required" := Some(codec.required).filter(_.nonEmpty)
      ).dropNullValues
    case codec: Schema.Value =>
      JsonObject(
        "type" := codec.tpe,
        "format" := codec.format,
        "description" := codec.description,
        "additionalProperties" := codec.additionalProperties
      ).dropNullValues

  given Encoder.AsObject[SecurityRequirement] = _.toChain.asJsonObject

  given Encoder.AsObject[Server] = server =>
    JsonObject(
      "url" := server.url,
      "description" := server.description,
      "variables" := Some(server.variables).filter(_.nonEmpty)
    ).dropNullValues

  given Encoder.AsObject[ServerVariable] = serverVariable =>
    JsonObject(
      "default" := serverVariable.default,
      "enum" := Some(serverVariable.enums).filter(_.nonEmpty),
      "description" := serverVariable.description
    ).dropNullValues

  given Encoder.AsObject[Tag] = tag =>
    JsonObject(
      "name" := tag.name,
      "description" := tag.description,
      "externalDocs" := tag.externalDocs
    ).dropNullValues
