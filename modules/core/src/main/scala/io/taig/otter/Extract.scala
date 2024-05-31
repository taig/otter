package io.taig.otter

final case class Extract[+F[_], A](fa: F[A]) extends AnyVal
