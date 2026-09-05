package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Result
import io.taig.otter.http.Results
import io.taig.validation.Violation

/** Reads the answer an endpoint gave, choosing the branch the status code names.
  *
  * No new machinery selects the branch. [[UnionDecoder]] already tries them in turn, and [[Result.Value.Root]] already
  * carries the [[io.taig.otter.http.Code]] its branch answers under, so a `Root` that refuses a status it was not
  * written for is all it takes for the union to sort itself out.
  *
  * The one thing trying cannot do well is report. `UnionDecoder` combines its branches with `orElse`, which keeps the
  * last failure and discards the rest, so a response under a status no branch names would be reported as whatever the
  * final branch happened to object to. That case is therefore caught before the union is entered, and named for what it
  * is.
  */
final class Http4sResultDecoder(payload: Http4sPayload) extends Decoder[Results.Node, Http4sWire.Response]:
  private val results = UnionDecoder(Http4sResultDecoder.One(payload))

  override def decode[R](results: Results.Node[Nothing, R], value: Http4sWire.Response): Validated[Violations, R] =
    val codes = results.self.self.branches.map(_.value.code)

    if codes.exists(_ == value.code) then this.results.decode(results.self.self, value)
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.OneOf(codes.toList.map(_.value.asData)),
          actual = value.code.value.asData,
          hint = none
        )
      ).invalid

object Http4sResultDecoder:
  /** One branch of the union, which answers only under the status it was written for. */
  final class One(payload: Http4sPayload) extends Decoder[Result.Node, Http4sWire.Response]:
    private val bodies = UnionDecoder(Http4sBodyDecoder(payload))

    override def decode[R](result: Result.Node[Nothing, R], value: Http4sWire.Response): Validated[Violations, R] =
      decode(result.self.self, value)

    private def decode[R](
        result: Result.Value[Body.Payload, Nothing, R],
        value: Http4sWire.Response
    ): Validated[Violations, R] = result match
      case Result.Value.Root(code) =>
        if code == value.code then ().valid
        else
          Violations(
            Violation(
              constraint = Constraint.Generic.Equals(code.value.asData),
              actual = value.code.value.asData,
              hint = none
            )
          ).invalid
      case Result.Value.Headers(self, headers) =>
        (decode(self, value), HeadersDecoder.decode(headers.value, value.headers).leftMap("header" /: _)).tupled
      case Result.Value.Payload(self, values) =>
        val body =
          value.body.fold((Option.empty, scodec.bits.ByteVector.empty))((mediaType, bytes) => (Some(mediaType), bytes))

        (decode(self, value), bodies.decode(values.value.self.self, body).leftMap("body" /: _)).tupled
      case Result.Value.Streaming(self, _) => decode(self, value)
      case Result.Value.Modify(self, f, _) => decode(self, value).map(f)
