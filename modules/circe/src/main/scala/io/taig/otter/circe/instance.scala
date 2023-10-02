package io.taig.otter.circe

import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.Data
import io.taig.otter.openapi.*
import io.taig.otter.circe.syntax.*

object instance:
  given [A <: Data]: Encoder[A] = fromData(_)
  given Encoder.AsObject[Data.Object] = fromData(_)

  given Encoder.AsObject[Contact] = contact =>
    JsonObject(
      "name" := contact.name,
      "url" := contact.url,
      "email" := contact.email
    ).dropNullValues

  given [A: Encoder.AsObject]: Encoder.AsObject[Extended[A]] = extended =>
    extended.value.asJsonObject ++ extended.extensions.asJsonObject

  given Encoder.AsObject[Extensions] = extensions =>
    JsonObject.fromFoldable(extensions.toChain.map { case (key, value) => (s"x-$key", value.asJson) })

  given Encoder.AsObject[ExternalDocumentation] = documentation =>
    JsonObject(
      "description" := documentation.description,
      "url" := documentation.url
    ).dropNullValues

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
    given webhook: Encoder.AsObject[PathItem | Reference] =
      case pathItem: PathItem   => pathItem.asJsonObject
      case reference: Reference => reference.asJsonObject

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
    given parameterOrCallback: Encoder.AsObject[Extended[Data.Object] | Reference] =
      case parameter: Extended[Data.Object] => parameter.asJsonObject
      case reference: Reference             => reference.asJsonObject

    given requestBody: Encoder.AsObject[Extended[RequestBody] | Reference] =
      case request: Extended[RequestBody] => request.asJsonObject
      case reference: Reference           => reference.asJsonObject

    JsonObject(
      "tags" := operation.tags,
      "summary" := operation.summary,
      "description" := operation.description,
      "externalDocs" := operation.externalDocs,
      "operationId" := operation.operationId,
      "parameters" := Some(operation.parameters).filter(_.nonEmpty),
      "requestBody" := operation.requestBody,
      "responses" := operation.responses,
      "callbacks" := Some(operation.callbacks).filter(_.nonEmpty),
      "deprecated" := operation.deprecated,
      "security" := operation.security,
      "servers" := Some(operation.servers).filter(_.nonEmpty)
    ).dropNullValues

  given Encoder.AsObject[PathItem] = item =>
    given Encoder.AsObject[Extended[Data.Object] | Reference] =
      case parameter: Extended[Data.Object] => parameter.asJsonObject
      case reference: Reference             => reference.asJsonObject

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

  given Encoder.AsObject[Paths] = paths =>
    JsonObject.fromFoldable(paths.toChain.map { case (path, pathItem) => (path, pathItem.asJson) })

  given Encoder.AsObject[Reference] = reference =>
    JsonObject(
      "$ref" := reference.ref,
      "summary" := reference.summary,
      "description" := reference.description
    ).dropNullValues

  given Encoder.AsObject[RequestBody] = body =>
    JsonObject(
      "content" := body.content,
      "description" := body.description,
      "required" := body.required
    ).dropNullValues

  given Encoder.AsObject[Responses] = responses => JsonObject("default" := responses.default).dropNullValues

  given Encoder.AsObject[Schema] =
    case schema: Schema.Array =>
      JsonObject(
        "type" := "array",
        "format" := schema.format,
        "description" := schema.description,
        "items" := schema.items
      ).dropNullValues
    case schema: Schema.Enumeration => JsonObject("type" := schema.tpe, "enum" := schema.enums.map(_.asJson))
    case schema: Schema.OneOf       => JsonObject("oneOf" := schema.schemas)
    case schema: Schema.Object =>
      JsonObject(
        "type" := "object",
        "format" := schema.format,
        "description" := schema.description,
        "properties" := Some(schema.properties.map { case (name, value) => (name, value.asJson) })
          .filter(_.nonEmpty)
          .map(JsonObject.fromFoldable)
      ).dropNullValues
    case schema: Schema.Value =>
      JsonObject(
        "type" := schema.tpe,
        "format" := schema.format,
        "description" := schema.description
      ).dropNullValues

  given Encoder.AsObject[SecurityRequirement] = security =>
    JsonObject.fromFoldable(security.toChain.map { case (name, scopes) => (name, scopes.asJson) })

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
