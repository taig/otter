package io.taig.otter.http.syntax

import io.taig.otter.http.Endpoint
import io.taig.otter.http.Request
import io.taig.otter.http.Response

trait EndpointSyntax:
  def endpoint[S[_], A, B](request: Request[S, A], response: Response[S, B]): Endpoint[S, A, B] =
    Endpoint(request, response)

object EndpointSyntax extends EndpointSyntax
