package io.taig.openapi.http

import cats.syntax.all.*
import io.taig.openapi.schema.Violations

final class OpenApiHttpException(val violations: Violations) extends Exception:
  override def getMessage: String = ???

object OpenApiHttpException:
  def apply(violations: Violations): OpenApiHttpException = new OpenApiHttpException(violations)
