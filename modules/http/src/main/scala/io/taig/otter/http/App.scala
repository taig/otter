package io.taig.otter.http

final case class App[F[_]](
    routes: Routes[F],
    notFound: Response[Unit],
    failure: Response[Unit]
)
