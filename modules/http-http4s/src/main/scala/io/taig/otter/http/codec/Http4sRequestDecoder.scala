package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Request

/** Reads what a request holds out of the slices it arrived as.
  *
  * The walk is the request's own shape: every [[Request.Value]] but `Root` wraps another and contributes a second half,
  * so reading one is reading the rest and pairing the two. That the pairs nest exactly as the schema's `R` does is why
  * nothing here has to know how deep it is.
  *
  * Each position labels its own violations, which the codecs below cannot do for themselves: a query named `id` and a
  * path segment named `id` both report at `id`, and only this tier knows which of the two a report came from.
  */
final class Http4sRequestDecoder(payload: Http4sPayload) extends Decoder[Request.Node, Http4sWire.Request]:
  private val bodies = UnionDecoder(Http4sBodyDecoder(payload))

  override def decode[R](request: Request.Node[Nothing, R], value: Http4sWire.Request): Validated[Violations, R] =
    decode(request.self.self, value)

  private def decode[R](
      request: Request.Value[Body.Payload, Nothing, R],
      value: Http4sWire.Request
  ): Validated[Violations, R] = request match
    case Request.Value.Root(_, path)          => PathDecoder.decode(path.value, value.path).leftMap("path" /: _)
    case Request.Value.Queries(self, queries) =>
      (decode(self, value), QueriesDecoder.decode(queries.value, value.queries).leftMap("query" /: _)).tupled
    case Request.Value.Headers(self, headers) =>
      (decode(self, value), HeadersDecoder.decode(headers.value, value.headers).leftMap("header" /: _)).tupled
    case Request.Value.Payload(self, values) =>
      (decode(self, value), bodies.decode(values.value.self.self, value.body).leftMap("body" /: _)).tupled
    // A streamed body changes what the request describes and not what it holds, so there is nothing to read here.
    case Request.Value.Streaming(self, _) => decode(self, value)
    case Request.Value.Modify(self, f, _) => decode(self, value).map(f)
