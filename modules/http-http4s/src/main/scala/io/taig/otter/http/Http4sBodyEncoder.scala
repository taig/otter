package io.taig.otter.http

import io.taig.otter.http.header.Accept
import org.http4s.Entity as Http4sBody

final class Http4sBodyEncoder[F[_], S[_]](encoder: PayloadEncoder[S]):
  // TODO what should I actually pass here rather than charset?
  def apply[A](body: Body[S, A], accept: Option[Accept], a: A): Option[Http4sBody[F]] = ???
  // body match
  //   case Body.Empty                  => Http4sBody.empty.some
  //   case Body.Modify(self, _, g)     => apply(body = self, accept, g(a))
  //   case Body.Or(left, right)        => ???
  //   case Body.OrElse(left, right)    => a.fold(apply(body = left, accept, _), apply(body = right, accept, _))
  //   case Body.Root(mediaType, codec) =>
  //     // accept.forall(mediaType.satisfies(mediaRange))
  //     // TODO content negotion
  //     Http4sBody.strict(ByteVector(encoder(mediaType, codec.value, a))).some
