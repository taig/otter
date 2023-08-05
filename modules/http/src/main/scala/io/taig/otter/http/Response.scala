package io.taig.otter.http

import io.taig.otter.schema.Violations

final case class Response[+A](results: Results[A], violations: Result[Violations])
