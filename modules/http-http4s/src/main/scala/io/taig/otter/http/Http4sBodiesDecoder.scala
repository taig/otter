package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import org.http4s.Header as Http4sHeader
import org.typelevel.ci.*
import io.taig.otter.Violation

final class Http4sBodiesDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      bodies: Bodies[S, A],
      headers: List[Http4sHeader.Raw],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] = headers
    .find(_.name === ci"Content-Type")
    .map(_.value)
    .traverse: value =>
      MediaType
        .parse(value)
        .toValidated
        .leftMap: error =>
          println(error)
          Violations.rootNec(Violation.tpe(name = "Content-Type", actual = value, hint = error.show))
    .andThen(new BodiesDecoder(decoder)(codec = bodies, _, bytes))
