package io.taig.otter.http.operation

import io.taig.otter.InvariantK

trait PathOperation[F[_]]:
  extension [A](self: F[A]) def todo: String = "todo"

object PathOperation:
  given InvariantK[PathOperation] = ???
