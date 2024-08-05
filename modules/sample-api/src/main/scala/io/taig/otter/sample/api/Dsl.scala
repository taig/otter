package io.taig.otter.sample.api

import io.taig.otter.http.json as HttpJson
import io.taig.otter.openapi as Openapi
import org.typelevel.ci.*
import java.util.regex.Pattern
import cats.syntax.all.*
import io.taig.otter.sample.api.schema.SessionApiSchema

object Dsl extends HttpJson.Dsl, Openapi.Dsl:
  val email: Primitive.Required[CIString] = cistring(matches = Pattern.compile(".+@.+", Pattern.CASE_INSENSITIVE).some)

  extension (self: header.type)
    def session: Header[SessionApiSchema] =
      self.authorization(SessionApiSchema.codec(prefix = "Bearer "))

  extension [I, O](self: Endpoint[I, O])
    def role[R <: Role](role: R): AuthenticatedEndpoint[R, I, O] = AuthenticatedEndpoint(
      role,
      self
        .modifyRequest(request => (header.session.optional *: request).to[AuthenticationApiSchema[I]])
        .modifyResponse(_.modifyResults(AuthenticationApiSchema.codec.orElse))
    )
