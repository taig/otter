package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.Json
import io.taig.otter.http.Client
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.http.HttpError
import io.taig.otter.dsl.*
import cats.syntax.all.*
import scala.annotation.nowarn

final class TestClient(client: Client[IO, Json, Json, Json]):
  def submit[I, E, O](endpoint: Endpoint[I, E, O])(input: I): IO[Either[HttpError, Either[E, O]]] =
    client.submit(endpoint, contentType = mediaType.application.json.some, input)
