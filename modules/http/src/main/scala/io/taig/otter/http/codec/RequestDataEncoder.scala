package io.taig.otter.http.codec

import io.taig.otter.http.Request
import io.taig.otter.http.header.MediaType

final class RequestDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val bodies = BodiesEncoder(encoder)

  def encode[A](schema: Request[S, A], contentType: Option[MediaType], a: A): Request.Data = ???
  // schema match
  //   case Request.Modify(self, _, g) => encode(schema = self, contentType, g(a))
  //   case Request.Payload(self, bodies) =>
  //     encode(schema = self, contentType, a.init)
  //       .withBody(this.bodies.encode(bodies, contentType, a._3).getOrElse(Array.emptyByteArray))
  //   case Request.Root(method, url, headers) =>
  //     Request.Data(
  //       method,
  //       url = UrlDataEncoder.encode(url, a._1),
  //       headers = HeadersDataEncoder.encode(headers, a._2),
  //       body = Array.emptyByteArray
  //     )
  //   case Request.ZipHeaders(self, headers) =>
  //     encode(schema = self, contentType, a._1).modifyHeaders(_ ++ HeadersDataEncoder.encode(headers, a._2))
