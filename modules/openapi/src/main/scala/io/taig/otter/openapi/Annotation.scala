package io.taig.otter.openapi

final case class Annotation[+M, +A](metadata: M, self: A)
