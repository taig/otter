package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Request
import io.taig.otter.http.Response

trait EndpointSyntax:
  def endpoint[S[_], T[_], U[_], A, B](request: Request[S, A], response: Response[T, U, B]): Endpoint[S, T, U, A, B] =
    Endpoint(request, response, metadata = Metadata.Empty)

object EndpointSyntax extends EndpointSyntax
