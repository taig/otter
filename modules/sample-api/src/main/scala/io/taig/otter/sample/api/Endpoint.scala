package io.taig.otter.sample.api

import io.taig.otter.Json
import io.taig.otter.http.Endpoint as OtterEndpoint

type Endpoint[A, E, B] = OtterEndpoint[Json, Json, Json, A, Either[E, B]]
