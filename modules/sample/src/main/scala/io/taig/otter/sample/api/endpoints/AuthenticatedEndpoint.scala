package io.taig.otter.sample.api.endpoints

import io.taig.otter.http.Endpoint
import io.taig.otter.sample.api.{Authentication, Role}

final case class AuthenticatedEndpoint[R <: Role, I, O](
    role: R,
    toAuthenticatedEndpoint: Endpoint[Authentication[I], Either[Authentication.Error, O]]
)
