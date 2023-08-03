package io.taig.crock.http

import cats.data.Chain

type Routes[F[_]] = Chain[Endpoint.Implementation[F, ?, ?]]
