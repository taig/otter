package io.taig.otter.sample

import cats.effect.IO
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.http.Client
import io.taig.otter.http.HttpError
import io.taig.otter.sample.api.Endpoint

final class TestClient(client: Client[IO, Json, Json, Json]):
  def submit[I, E, O](endpoint: Endpoint[I, E, O])(input: I): IO[Either[HttpError, Either[E, O]]] =
    client.submit(endpoint, contentType = mediaType.application.json.some, input)
