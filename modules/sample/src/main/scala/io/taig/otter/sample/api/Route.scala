package io.taig.otter.sample.api

import cats.effect.IO
import io.taig.otter.http.Route as OtterRoute

type AuthenticatedRoute[I, O] = OtterRoute[IO, Authentication[I], Either[Authentication.Error, O]]
