package io.taig.otter.http.codec

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Union
import io.taig.otter.Violations
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Http4sFailure
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
final class Http4sResponseDecoder[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](
    payload: Http4sInterpreter.Of[P, Q],
    endpoint: Option[Endpoint.Node] = None
):
  private val response = Http4sResponseDecoder.One[F, P, Q](payload, endpoint)

  def contextual(endpoint: Endpoint.Node): Http4sResponseDecoder[F, P, Q] =
    new Http4sResponseDecoder(payload, Some(endpoint))

  def decode[R](
      schema: Responses.Schema[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
      value: Http4sWire.Response[F]
  ): F[Validated[Violations, R]] =
    val statuses = schema.self.self.branches.map(_.value.status)

    if statuses.exists(_ == value.status) then
      val matching = schema.self.self.branches.map(_.value).toList.filter(_.status == value.status)
      val choices = matching.flatMap(
        _.bodies.toList.flatMap(reference => io.taig.otter.http.Bodies.branches(reference.value).toList)
      )
      val eligible = choices.filter(body => value.body._1.forall(_.essence == body.mediaType.essence))
      if eligible.size > 1 && eligible.exists(body => io.taig.otter.http.Http4sStreaming.isStream(body.self.self)) then
        Violations(Violation(Constraint.Generic.Type("unambiguous stream content type"), io.taig.data.Data.Null, None))
          .invalid[R]
          .pure[F]
      else if eligible.isEmpty || eligible.exists(body =>
          io.taig.otter.http.Http4sStreaming.isStream(body.self.self)
        ) || choices.isEmpty
      then responses(schema.self.self, value)
      else
        io.taig.otter.http.Http4sEnvelope
          .toBytes(value.body._2)
          .flatMap(bytes =>
            responses(schema.self.self, value.copy(body = (value.body._1, org.http4s.Entity.strict(bytes))))
          )
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.OneOf(statuses.toList.map(_.value.asData)),
          actual = value.status.value.asData,
          hint = none
        )
      ).invalid.pure[F]

  private def responses[R](
      schema: Union[Response.Schema[Http4sInterpreter.Supported[F, P, Q], *, *], Nothing, R],
      value: Http4sWire.Response[F]
  ): F[Validated[Violations, R]] = schema match
    case Union.Root(branch)           => response.decode(branch.value, value)
    case Union.Modify(self, f, _)     => responses(self, value).map(_.map(f))
    case Union.Coproduct(left, right) =>
      responses(left, value).flatMap:
        case Validated.Valid(result) => Validated.valid(Left(result)).pure[F]
        case Validated.Invalid(_)    => responses(right, value).map(_.map(Right(_)))

private[http] object Http4sResponseDecoder:
  /** One branch of the union, which answers only under the status it was written for. */
  final private[http] class One[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](
      payload: Http4sInterpreter.Of[P, Q],
      endpoint: Option[Endpoint.Node] = None
  ):
    private val bodies = Http4sBodyDecoder[F, P, Q](payload, endpoint.map((_, Http4sFailure.Direction.Response)))

    def decode[R](
        response: Response.Schema[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
        value: Http4sWire.Response[F]
    ): F[Validated[Violations, R]] =
      if response.status == value.status then decode(response.self.self, value)
      else
        Violations(Violation(Constraint.Generic.Equals(response.status.value.asData), value.status.value.asData, None))
          .invalid[R]
          .pure[F]

    private def decode[R](
        response: Response.Value[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
        value: Http4sWire.Response[F]
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
          value.body

        (
          decode(self, value),
          bodies.bodies(values.value.self.self, body).map(_.leftMap(failure => "body" /: failure.violations))
        ).mapN((left, right) => (left, right).tupled)
      case Response.Value.Modify(self, f, _) => decode(self, value).map(_.map(f))
