package io.taig.otter.sample.api

import io.taig.otter.http.json as Json
import io.taig.otter.http.csv as Csv
import io.taig.otter.openapi as Openapi
import cats.syntax.all.*
import io.taig.otter.sample.api as Api

object Dsl extends Csv.Dsl, Json.Dsl, Openapi.Dsl, Codecs:
  given Conversion[header.type, Api.Headers] = _ => new Api.Headers {}

  extension [I, O](self: Endpoint[I, O])
    def role[R <: Role](role: R): AuthenticatedEndpoint[R, I, O] = AuthenticatedEndpoint(
      role,
      self
        .modifyRequest(request => (header.session.optional *: request).to[AuthenticationApiSchema[I]])
        .modifyResponse(_.modifyResults(AuthenticationApiSchema.results.orElse))
    )
