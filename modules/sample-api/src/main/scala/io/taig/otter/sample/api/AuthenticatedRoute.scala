package io.taig.otter.sample.api

import io.taig.otter.http.Route

type AuthenticatedRoute[F[_], I, O] = Route[F, AuthenticationApiSchema[I], Either[AuthenticationApiSchema.Error, O]]
