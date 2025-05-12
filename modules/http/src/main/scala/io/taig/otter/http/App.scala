package io.taig.otter.http

final case class App[F[_], S[_], T[_], U[_]](routes: Routes[F, S, T, U], notFound: Results[U, Unit])
