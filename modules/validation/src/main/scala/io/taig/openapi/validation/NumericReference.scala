package io.taig.openapi.validation

final case class NumericReference[A](value: A, equal: Boolean, delta: Option[A])
