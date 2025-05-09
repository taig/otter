package io.taig.otter.http

import cats.data.Chain

type Routes[F[_], +S[_], +T[_], +U[_]] = Chain[Route[F, S, T, U, ?, ?]]

object Routes:
  export Chain.*
