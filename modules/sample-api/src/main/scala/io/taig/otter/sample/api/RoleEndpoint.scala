// package io.taig.otter.sample.api

// import io.taig.otter.http.Endpoint

// final case class RoleEndpoint[R <: Role, I, O](
//     role: R,
//     toAuthenticatedEndpoint: Endpoint[AuthenticationApiSchema[I], Either[AuthenticationApiSchema.Error, O]]
// )
