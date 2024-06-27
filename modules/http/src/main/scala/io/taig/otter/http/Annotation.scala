package io.taig.otter.http

final case class Annotation[+M, +A](metadata: M, self: A)
