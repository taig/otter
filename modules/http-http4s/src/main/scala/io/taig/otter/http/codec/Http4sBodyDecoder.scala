package io.taig.otter.http.codec

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Union
import io.taig.otter.Violations
import io.taig.otter.http.Body
import io.taig.otter.http.DecodingFailure
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4sEnvelope
import io.taig.otter.http.Http4sFailure
import io.taig.otter.http.Http4sStreaming
import io.taig.otter.http.MediaType
import io.taig.validation.Violation
import org.http4s.Entity

/** Reads a body out of the bytes that arrived, and the media type they arrived under.
  *
  * The media type is half the input because a [[io.taig.otter.http.Bodies]] is a union, and a union decoder picks its
  * branch by trying them: without a type to disagree with, the first alternative that could parse the bytes would win
  * whatever the sender said they were. With it, `application/json` and `application/pdf` sort themselves out and
  * [[io.taig.otter.codec.UnionDecoder]] needs no help.
  *
  * `Option`al because a request may carry a body and no `Content-Type`, and there is nothing to be gained by refusing
  * one when exactly one streamed alternative is eligible. Multiple eligible alternatives involving streams require a
  * content type; consuming a stream to try another alternative would lose its lifetime and backpressure.
  *
  * The walk is written at the requirement the interpreter covers rather than at [[Body.Node]], which is what lets a
  * [[Body.Value.Whole]] hand its payload over as a `P` instead of as an `Any`. Stream elements similarly reach the
  * separately registered alphabet `Q`, with the carrier fixed to fs2's stream in this effect.
  */
final class Http4sBodyDecoder[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](
    payload: Http4sInterpreter.Of[P, Q],
    context: Option[(Endpoint.Node, Http4sFailure.Direction)] = None
):
  private def contextual[A](stream: fs2.Stream[F, A]): fs2.Stream[F, A] =
    context.fold(stream)((endpoint, direction) => Http4sStreaming.context(endpoint, direction)(stream))

  def decode[R](
      schema: Body.Schema[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
      value: (Option[MediaType], Entity[F])
  ): F[Validated[Violations, R]] = decodeDetailed(schema, value).map(_.leftMap(_.violations))

  /** Retains the error category as well as its structured violations. */
  def decodeDetailed[R](
      schema: Body.Schema[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
      value: (Option[MediaType], Entity[F])
  ): F[Validated[DecodingFailure, R]] = decode(schema.self.self, value)

  private[http] def bodies[R](
      schema: Union[Body.Schema[Http4sInterpreter.Supported[F, P, Q], *, *], Nothing, R],
      value: (Option[MediaType], Entity[F])
  ): F[Validated[DecodingFailure, R]] =
    val branches = schema.branches.map(_.value).toList
    val eligible = branches.filter(body => value._1.forall(_.essence == body.mediaType.essence))
    if eligible.size > 1 && eligible.exists(body => Http4sStreaming.isStream(body.self.self)) then
      DecodingFailure(
        Failure.Category.ContentType,
        Violations(Violation(Constraint.Generic.Type("unambiguous stream content type"), Data.Null, None))
      ).invalid[R].pure[F]
    else if eligible.isEmpty || eligible.exists(body => Http4sStreaming.isStream(body.self.self)) then
      alternatives(schema, value)
    else Http4sEnvelope.toBytes(value._2).flatMap(bytes => alternatives(schema, (value._1, Entity.strict(bytes))))

  private def alternatives[R](
      schema: Union[Body.Schema[Http4sInterpreter.Supported[F, P, Q], *, *], Nothing, R],
      value: (Option[MediaType], Entity[F])
  ): F[Validated[DecodingFailure, R]] = schema match
    case Union.Root(branch)           => decodeDetailed(branch.value, value)
    case Union.Modify(self, f, _)     => alternatives(self, value).map(_.map(f))
    case Union.Coproduct(left, right) =>
      alternatives(left, value).flatMap:
        case Validated.Valid(result)    => Validated.valid(Left(result)).pure[F]
        case Validated.Invalid(failure) => alternatives(right, value).map(_.map(Right(_)).leftMap(failure |+| _))

  private def decode[R](
      body: Body.Value[Http4sInterpreter.Supported[F, P, Q], Nothing, R],
      value: (Option[MediaType], Entity[F])
  ): F[Validated[DecodingFailure, R]] =
    val (mediaType, entity) = value

    body match
      case Body.Value.Modify(self, f, _)         => decode(self, value).map(_.map(f))
      case Body.Value.Whole(declared, reference) =>
        Http4sBodyDecoder.matches(declared, mediaType) match
          case Validated.Valid(_) =>
            Http4sEnvelope.toBytes(entity).flatMap(payload.buffered.decode(reference.value, mediaType, _))
          case Validated.Invalid(failure) => Validated.invalid[DecodingFailure, R](failure).pure[F]
      case Body.Value.Binary(declared) =>
        Http4sBodyDecoder.matches(declared, mediaType) match
          case Validated.Valid(_)         => Http4sEnvelope.toBytes(entity).map(_.valid)
          case Validated.Invalid(failure) => failure.invalid[R].pure[F]
      case Body.Value.Streamed(declared, frame, reference) =>
        Http4sBodyDecoder
          .matches(declared, mediaType)
          .map(_ => contextual(payload.streams.decode(reference.value, frame, entity.body)))
          .pure[F]
      case Body.Value.Raw(declared) =>
        Http4sBodyDecoder.matches(declared, mediaType).map(_ => contextual(entity.body)).pure[F]

private[http] object Http4sBodyDecoder:
  /** Whether bytes announced as `actual` may be read as `declared`.
    *
    * On `essence`, so that `application/json; charset=utf-8` is `application/json`. The parameters a media type keeps
    * say how bytes became text and where a part ends, which is not what tells two bodies apart.
    */
  private[http] def matches(declared: MediaType, actual: Option[MediaType]): Validated[DecodingFailure, Unit] =
    if actual.forall(_.essence == declared.essence) then ().valid
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.Type(declared.essence.render),
          actual = actual.fold(Data.Null)(mediaType => mediaType.render.asData),
          hint = none
        )
      ).invalid.leftMap(violations => DecodingFailure(Failure.Category.ContentType, violations))
