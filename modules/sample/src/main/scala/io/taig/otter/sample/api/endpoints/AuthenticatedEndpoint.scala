package io.taig.otter.sample.api.endpoints

import io.taig.otter.http.Endpoint
import io.taig.otter.sample.api.{Authentication, Role}
import io.taig.otter.sample.api.headers

final case class AuthenticatedEndpoint[R <: Role, I, O](
    role: R,
    toAuthenticatedEndpoint: Endpoint[Authentication[I], Either[Authentication.Error, O]]
)

extension [I, O](self: Endpoint[I, O])
  def role[R <: Role](role: R): AuthenticatedEndpoint[R, I, O] = AuthenticatedEndpoint(
    role,
    self
      .modifyRequest(request => (headers.session.optional *: request).to[Authentication[I]])
      .modifyResponse(_.modifyResults(Authentication.codec.orElse))
  )
