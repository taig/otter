package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Union
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Body
import io.taig.otter.http.DecodingFailure
import io.taig.otter.http.Failure
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
final private[http] class Http4sRequestDecoderUnchecked(payload: Http4sPayload[?])
    extends Decoder[Request.Node, Http4sWire.Request]:
  private val body = Http4sBodyDecoderUnchecked(payload)

  private def bodies[R](
      schema: Union[Body.Node, Nothing, R],
      value: (Option[io.taig.otter.http.MediaType], scodec.bits.ByteVector)
  ): Validated[DecodingFailure, R] = schema match
    case Union.Root(branch)           => body.decodeDetailed(branch.value, value)
    case Union.Modify(self, f, _)     => bodies(self, value).map(f)
    case Union.Coproduct(left, right) =>
      bodies(left, value).map(Left(_)) match
        case Validated.Valid(result)    => Validated.valid(result)
        case Validated.Invalid(failure) => bodies(right, value).map(Right(_)).leftMap(failure |+| _)

  override def decode[R](request: Request.Node[Nothing, R], value: Http4sWire.Request): Validated[Violations, R] =
    decodeDetailed(request, value).leftMap(
      _.violations
    )

  private[http] def decodeDetailed[R](
      request: Request.Node[Nothing, R],
      value: Http4sWire.Request
  ): Validated[DecodingFailure, R] = decode(request.self.self, value)

  private def envelope(violations: Violations): DecodingFailure = DecodingFailure(Failure.Category.Envelope, violations)

  private def atBody(failure: DecodingFailure): DecodingFailure =
    failure.copy(violations = "body" /: failure.violations)

  private def decode[R](
      request: Request.Value[Body.Payload, Nothing, R],
      value: Http4sWire.Request
  ): Validated[DecodingFailure, R] = request match
    case Request.Value.Root(_, path) =>
      PathDecoder.decode(path.value, value.path).leftMap(violations => envelope("path" /: violations))
    case Request.Value.Queries(self, queries) =>
      (
        decode(self, value),
        QueriesDecoder.decode(queries.value, value.queries).leftMap(violations => envelope("query" /: violations))
      ).tupled
    case Request.Value.Headers(self, headers) =>
      (
        decode(self, value),
        HeadersDecoder.decode(headers.value, value.headers).leftMap(violations => envelope("header" /: violations))
      ).tupled
    case Request.Value.Entity(self, values) =>
      (decode(self, value), bodies(values.value.self.self, value.body).leftMap(atBody)).tupled
    case Request.Value.OptionalEntity(self, values) =>
      val body =
        if value.body._1.isEmpty && value.body._2.isEmpty then Validated.valid(None)
        else bodies(values.value.self.self, value.body).map(Some(_)).leftMap(atBody)

      (decode(self, value), body).tupled
    // A streamed body changes what the request describes and not what it holds, so there is nothing to read here.
    case Request.Value.Streamed(self, _)  => decode(self, value)
    case Request.Value.Modify(self, f, _) => decode(self, value).map(f)
