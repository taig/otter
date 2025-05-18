package io.taig.otter.http.codec

import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Request

final class RequestDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val bodies = BodiesEncoder(encoder)

  def encode[A](request: Request[S, A], contentType: Option[MediaType], a: A): Request.Data = request match
    case Request.Modify(self, _, g) => encode(request = self, contentType, g(a))
    case Request.Payload(self, bodies) =>
      encode(request = self, contentType, a.init)
        .withBody(this.bodies.encode(bodies, contentType, a._3).getOrElse(Array.emptyByteArray))
    case Request.Root(method, url, headers) =>
      Request.Data(
        method,
        url = UrlDataEncoder.encode(url, a._1),
        headers = HeadersDataEncoder.encode(headers, a._2),
        body = Array.emptyByteArray
      )
    case Request.ZipHeaders(self, headers) =>
      encode(request = self, contentType, a._1).modifyHeaders(_ ++ HeadersDataEncoder.encode(headers, a._2))
