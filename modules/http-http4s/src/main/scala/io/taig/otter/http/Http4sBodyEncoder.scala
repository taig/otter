package io.taig.otter.http

import org.http4s.Request as Http4sRequest
// import org.http4s.EntityBody as Http4sBody
import org.http4s.Entity as Http4sBody
import io.taig.otter.Encoder
import io.taig.otter.Reference
import scodec.bits.ByteVector
import java.nio.charset.StandardCharsets

// TODO this is nasty, how can I solve it?
final class Http4sBodyEncoder[F[_], S[_]](encode: Reference[?, ?] => String) extends Encoder[Body, Http4sBody[F]]:
  override def apply[A](body: Body[A], a: A): Http4sBody[F] = body match
    case Body.Empty(_) => Http4sBody.empty
    case Body.Modify(self, _, g) => apply(body = self, g(a))
    case Body.Or(left, right) => ???
    case Body.OrElse(left, right) => ???
    case Body.Root(mediaType, codec) =>
      val bytes = ByteVector(encode(codec).getBytes(StandardCharsets.UTF_8))
      Http4sBody.strict(bytes)
  