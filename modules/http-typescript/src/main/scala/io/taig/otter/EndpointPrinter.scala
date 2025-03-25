package io.taig.otter

import io.taig.otter.http.Endpoint

abstract class EndpointPrinter[F[_]]:
  def print(endpoint: Endpoint[?, ?]): F[String]
