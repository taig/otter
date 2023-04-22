package io.taig.openapi.sample

import cats.MonadThrow
import cats.effect.IO
import io.taig.openapi.http.{Client, Routes, RoutesClient}
import io.taig.openapi.http4s.*

final class Clients(val underlying: Client[IO], val unauthorized: UnauthorizedClient, val authorized: AuthorizedClient)

object Clients:
  def apply(client: Client[IO]): Clients = new Clients(client, UnauthorizedClient(client), AuthorizedClient(client))

  def default(routes: Routes[IO]): Clients = Clients(RoutesClient(routes))
