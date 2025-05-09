package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import org.http4s.Method as Http4sMethod

object Http4sMethodDecoder:
  def apply(method: Http4sMethod): Validated[Violations, Method] = method match
    case Http4sMethod.DELETE  => Method.Delete.valid
    case Http4sMethod.GET     => Method.Get.valid
    case Http4sMethod.HEAD    => Method.Head.valid
    case Http4sMethod.OPTIONS => Method.Options.valid
    case Http4sMethod.PATCH   => Method.Patch.valid
    case Http4sMethod.POST    => Method.Post.valid
    case Http4sMethod.PUT     => Method.Put.valid
    case Http4sMethod.TRACE   => Method.Trace.valid
    case _ =>
      val values = Method.mapping.values.toList.map(Method.mapping.apply)
      Violations.rootNec(Violation.oneOf(values, actual = method.name)).invalid
