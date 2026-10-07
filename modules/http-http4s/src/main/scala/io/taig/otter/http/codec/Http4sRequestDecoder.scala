package io.taig.otter.http.codec

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.Violations
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
  *
  * A request carrying a streamed body is not a case here. Its requirement is [[Body.Requirement.Streamed]], which no
  * `Supported[P]` admits, so such a request cannot be served by this backend and cannot reach this walk.
  */
final class Http4sRequestDecoder[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload[P]):
  private val body = Http4sBodyDecoder[F, P](payload)

  def decode[R](
      schema: Request.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Request
  ): F[Validated[Violations, R]] = decodeDetailed(schema, value).map(_.leftMap(_.violations))

  /** Retains the error category as well as its structured violations. */
  def decodeDetailed[R](
      schema: Request.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Request
  ): F[Validated[DecodingFailure, R]] = decode(schema.self.self, value)

  private def envelope(violations: Violations): DecodingFailure = DecodingFailure(Failure.Category.Envelope, violations)

  private def atBody(failure: DecodingFailure): DecodingFailure =
    failure.copy(violations = "body" /: failure.violations)

  private def decode[R](
      request: Request.Value[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Request
  ): F[Validated[DecodingFailure, R]] = request match
    case Request.Value.Root(_, path) =>
      PathDecoder.decode(path.value, value.path).leftMap(violations => envelope("path" /: violations)).pure[F]
    case Request.Value.Queries(self, queries) =>
      (
        decode(self, value),
        QueriesDecoder
          .decode(queries.value, value.queries)
          .leftMap(violations => envelope("query" /: violations))
          .pure[F]
      ).mapN((left, right) => (left, right).tupled)
    case Request.Value.Headers(self, headers) =>
      (
        decode(self, value),
        HeadersDecoder
          .decode(headers.value, value.headers)
          .leftMap(violations => envelope("header" /: violations))
          .pure[F]
      ).mapN((left, right) => (left, right).tupled)
    case Request.Value.Entity(self, values) =>
      (decode(self, value), body.bodies(values.value.self.self, value.body).map(_.leftMap(atBody))).mapN(
        (left, right) => (left, right).tupled
      )
    case Request.Value.OptionalEntity(self, values) =>
      val decoded =
        if value.body._1.isEmpty && value.body._2.isEmpty then none.pure[[a] =>> Validated[DecodingFailure, a]].pure[F]
        else body.bodies(values.value.self.self, value.body).map(_.map(Some(_)).leftMap(atBody))

      (decode(self, value), decoded).mapN((left, right) => (left, right).tupled)
    case Request.Value.Modify(self, f, _) => decode(self, value).map(_.map(f))
