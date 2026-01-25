package io.taig.otter.http.operation

import io.taig.otter.http.Parameter

trait SegmentOperation[F[_]]:
  def lift(name: String): F[Unit]

  def lift[A](name: String, schema: Parameter[A]): F[A]
