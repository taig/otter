package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import org.http4s.Header as Http4sHeader
import org.typelevel.ci.*
import io.taig.otter.Violation
import org.http4s.Entity as Http4sBody
import scodec.bits.ByteVector
import io.taig.otter.http.header.Accept

final class Http4sBodiesEncoder[F[_], -S[_]](encoder: PayloadEncoder[S]):
  def apply[A](bodies: Bodies[S, A], accept: Option[Accept], a: A): Option[Http4sBody[F]] =
    BodiesEncoder(encoder)(bodies, accept, a).map(ByteVector.apply).map(Http4sBody.strict)
