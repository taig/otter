package io.taig.otter.http

import io.taig.otter.validation.Violations

final class ViolationsException(val violations: Violations) extends RuntimeException
