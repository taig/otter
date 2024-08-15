package io.taig.otter.client

import io.taig.otter.http.Http

abstract class Client[F[_]]:
  def submit(request: Http.Request): F[Http.Response]
