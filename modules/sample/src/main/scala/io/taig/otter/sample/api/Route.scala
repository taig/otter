package io.taig.otter.sample.api

import cats.effect.IO
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.Authentication

import java.util.UUID

type Route[R, I, O] = OtterRoute[IO, Authentication[R, UUID, I], Either[Authentication.Error, O]]
