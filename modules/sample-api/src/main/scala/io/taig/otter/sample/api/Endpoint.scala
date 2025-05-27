package io.taig.otter.sample.api

import io.taig.otter.Json
import io.taig.otter.http.Endpoint as OtterEndpoint

type Endpoint[I, E, O] = OtterEndpoint[Json, I, Either[E, O]]
