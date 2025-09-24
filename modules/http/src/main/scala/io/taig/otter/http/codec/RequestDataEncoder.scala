package io.taig.otter.http.codec

import io.taig.otter.http.Request
import io.taig.otter.http.header.MediaType

final class RequestDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val bodies = BodiesEncoder(encoder)

  def encode[A](schema: Request[S, A], contentType: Option[MediaType], a: A): Request.Data =
    encodeValue(schema = schema.value, contentType, a)

  private def encodeValue[A](schema: Request.Value[S, A], contentType: Option[MediaType], a: A): Request.Data =
    schema match
      case Request.Value.Modify(self, _, g)    => encodeValue(schema = self, contentType, g(a))
      case Request.Value.Payload(self, bodies) =>
        encodeValue(schema = self, contentType, a.init)
          .withBody(this.bodies.encode(bodies, contentType, a._3).getOrElse(Array.emptyByteArray))
      case Request.Value.Root(method, url, headers) =>
        Request.Data(
          method,
          url = UrlDataEncoder.encode(url, a._1),
          headers = HeadersDataEncoder.encode(headers, a._2),
          body = Array.emptyByteArray
        )
      case Request.Value.ZipHeaders(self, headers) =>
        encodeValue(schema = self, contentType, a._1).modifyHeaders(_ ++ HeadersDataEncoder.encode(headers, a._2))
