package io.taig.otter.sample.api

import io.taig.otter.http.Endpoint as OtterEndpoint
import io.taig.otter.http.FormData
import io.taig.otter.+
import io.taig.otter.Json

type Endpoint[A, E, B] = OtterEndpoint[Json + FormData, Json, Json, A, Either[E, B]]
