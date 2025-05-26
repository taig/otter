package io.taig.otter.http

import cats.data.Chain

type Routes[F[_], +S[_, _]] = Chain[Route[F, S, ?, ?]]

object Routes:
  export Chain.*
