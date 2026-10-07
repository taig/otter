package io.taig.otter.http.codec

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Union
import io.taig.otter.Violations
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Response
import io.taig.otter.http.Responses
import io.taig.validation.Violation

/** Reads the answer an endpoint gave, choosing the branch the status names.
  *
  * No new machinery selects the branch. [[UnionDecoder]] already tries them in turn, and [[Response.Value.Root]]
  * already carries the [[io.taig.otter.http.Status]] its branch answers under, so a `Root` that refuses a status it was
  * not written for is all it takes for the union to sort itself out.
  *
  * The one thing trying cannot do well is report. `UnionDecoder` combines its branches with `orElse`, which keeps the
  * last failure and discards the rest, so a response under a status no branch names would be reported as whatever the
  * final branch happened to object to. That case is therefore caught before the union is entered, and named for what it
  * is.
  */
final class Http4sResponseDecoder[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload[P]):
  private val response = Http4sResponseDecoder.One[F, P](payload)

  def decode[R](
      schema: Responses.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Response
  ): F[Validated[Violations, R]] =
    val statuses = schema.self.self.branches.map(_.value.status)

    if statuses.exists(_ == value.status) then responses(schema.self.self, value)
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.OneOf(statuses.toList.map(_.value.asData)),
          actual = value.status.value.asData,
          hint = none
        )
      ).invalid.pure[F]

  private def responses[R](
      schema: Union[Response.Schema[Http4sPayload.Supported[P], *, *], Nothing, R],
      value: Http4sWire.Response
  ): F[Validated[Violations, R]] = schema match
    case Union.Root(branch)           => response.decode(branch.value, value)
    case Union.Modify(self, f, _)     => responses(self, value).map(_.map(f))
    case Union.Coproduct(left, right) =>
      responses(left, value).flatMap:
        case Validated.Valid(result) => Validated.valid(Left(result)).pure[F]
        case Validated.Invalid(_)    => responses(right, value).map(_.map(Right(_)))

private[http] object Http4sResponseDecoder:
  /** One branch of the union, which answers only under the status it was written for. */
  final private[http] class One[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload[P]):
    private val bodies = Http4sBodyDecoder[F, P](payload)

    def decode[R](
        response: Response.Schema[Http4sPayload.Supported[P], Nothing, R],
        value: Http4sWire.Response
    ): F[Validated[Violations, R]] = decode(response.self.self, value)

    private def decode[R](
        response: Response.Value[Http4sPayload.Supported[P], Nothing, R],
        value: Http4sWire.Response
    ): F[Validated[Violations, R]] = response match
      case Response.Value.Root(status) =>
        if status == value.status then ().valid[Violations].pure[F]
        else
          Violations(
            Violation(
              constraint = Constraint.Generic.Equals(status.value.asData),
              actual = value.status.value.asData,
              hint = none
            )
          ).invalid.pure[F]
      case Response.Value.Headers(self, headers) =>
        (decode(self, value), HeadersDecoder.decode(headers.value, value.headers).leftMap("header" /: _).pure[F]).mapN(
          (left, right) => (left, right).tupled
        )
      case Response.Value.Entity(self, values) =>
        val body =
          value.body.fold((Option.empty, scodec.bits.ByteVector.empty))((mediaType, bytes) => (Some(mediaType), bytes))

        (
          decode(self, value),
          bodies.bodies(values.value.self.self, body).map(_.leftMap(failure => "body" /: failure.violations))
        ).mapN((left, right) => (left, right).tupled)
      case Response.Value.Modify(self, f, _) => decode(self, value).map(_.map(f))
