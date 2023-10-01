package io.taig.otter.sample.api

import cats.effect.IO
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.Authentication

import java.util.UUID

type Route[R <: Role, I, O] = OtterRoute[IO, R, Authentication[I], Either[Authentication.Error, O]]
