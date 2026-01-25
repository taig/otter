package io.taig.otter.http.operation

trait PathOperation[F[_]]:
  extension [A] (self: F[A]) def todo: String = "todo"