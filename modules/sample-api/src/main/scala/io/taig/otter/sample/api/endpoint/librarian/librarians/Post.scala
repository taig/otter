package io.taig.otter.sample.api.endpoint.librarian.librarians

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Json
import io.taig.otter.http.Result
import io.taig.otter.dsl.*
import io.taig.otter.dsl.json.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*
import io.taig.otter.http.Response
import io.taig.otter.http.FormData

private val errors: Result[Json, Response.Error] = (
  result(
    code.notAcceptable,
    json(error("contentNegotationFailed"))
  ).as(Response.Error.ContentNegotiationFailed) :+
    // result(
    //   code.unsupportedMediaTypes,
    //   json(error("mediaTypesUnsupported", field("violations", violations)))
    // ).to[Response.Error.MediaTypesUnsupported] :+
    result(
      code.unprocessableEntity,
      json(error("validationViolations", field("violations", violations)))
    ).to[Response.Error.ValidationViolations]
).to

val failure = result(code.internalServerError, json(string.nullable))

def response[S[_], A](value: Result[S, A]): Response[S, Json, A] = Response(result = value, errors, failure)

val post: Endpoint[LibrarianApiSchema.Create, LibrarianInitializationConflict, LibrarianApiSchema] = endpoint(
  request(
    method.post,
    url,
    json(LibrarianApiSchema.Create.codec).or(formData(LibrarianApiSchema.Create.formData))
  ),
  response(
    result(code.conflict, json(LibrarianInitializationConflict.codec)) :+
      result(code.created, json(LibrarianApiSchema.codec).or(formData(LibrarianApiSchema.formData)))
  )
)
