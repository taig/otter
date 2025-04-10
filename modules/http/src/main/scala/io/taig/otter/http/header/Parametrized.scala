package io.taig.otter.http.header

final case class Parametrized[A](self: A, parameters: List[Parameter])
