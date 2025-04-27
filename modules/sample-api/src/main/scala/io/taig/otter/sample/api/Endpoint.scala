package io.taig.otter.sample.api

import io.taig.otter.http.Endpoint as OtterEndpoint
import io.taig.otter.Json

type Endpoint[A, E, B] = OtterEndpoint[Json, Json, A, Either[E, B]]
