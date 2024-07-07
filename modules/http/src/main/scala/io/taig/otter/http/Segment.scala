package io.taig.otter.http

import io.taig.otter.Value

sealed trait Segment[+F[+_], +A]

object Segment:
  final case class Static(name: String) extends Segment[Nothing, Unit]

  final case class Parameter[F[+_], A](name: String, schema: F[Value.Required.Reader[F, ?, A]])
