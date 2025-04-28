package io.taig.otter.http

import io.taig.otter.Metadata

trait EndpointDsl:
  def endpoint[S[_], T[_], U[_], A, B](request: Request[S, A], response: Response[T, U, B]): Endpoint[S, T, U, A, B] =
    Endpoint(request, response, metadata = Metadata.Empty)

object EndpointDsl extends EndpointDsl
