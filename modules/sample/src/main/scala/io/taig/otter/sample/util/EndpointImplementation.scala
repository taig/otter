package io.taig.otter.sample.util

import cats.effect.IO
import io.taig.otter.http.{Endpoint, Route}

final class EndpointImplementation:
  def apply[I, O](endpoint: Endpoint[I, O])(f: I => IO[O]): Route[IO, I, O] = Route(endpoint, f)
