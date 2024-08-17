package io.taig.otter.http

import io.taig.otter.Violations

final case class Error(tpe: Error.Type, violations: Violations)

object Error:
  enum Type:
    case ContentNegotiationFailed
    case MediaTypesUnsupported
    case ValidationViolations
