package io.taig.otter.http

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Collector
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.Http4sRequestDecoder.Data
import org.http4s.Header as Http4sHeader
import org.http4s.Method as Http4sMethod
import org.http4s.Query as Http4sQuery
import org.http4s.Request as Http4sRequest
import org.http4s.Uri as Http4sUri

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
