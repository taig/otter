package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Body
import io.taig.otter.http.DecodingFailure
import io.taig.otter.http.Failure
import io.taig.otter.http.MediaType
import io.taig.validation.Violation
import scodec.bits.ByteVector

/** Reads a body out of the bytes that arrived, and the media type they arrived under.
  *
  * The media type is half the input because a [[io.taig.otter.http.Bodies]] is a union, and a union decoder picks its
  * branch by trying them: without a type to disagree with, the first alternative that could parse the bytes would win
  * whatever the sender said they were. With it, `application/json` and `application/pdf` sort themselves out and
  * [[io.taig.otter.codec.UnionDecoder]] needs no help.
  *
  * `Option`al because a request may carry a body and no `Content-Type`, and there is nothing to be gained by refusing
  * one: a caller that says nothing has not contradicted the schema, so every alternative stays eligible.
  *
  * The walk is written at the requirement the interpreter covers rather than at [[Body.Node]], which is what lets a
  * [[Body.Value.Whole]] hand its payload over as a `P` instead of as an `Any`. A streamed body is not a case here at
  * all: its requirement is [[Body.Requirement.Streamed]], which no `Supported[P]` admits, so the compiler already knows
  * one cannot arrive.
  */
final class Http4sBodyDecoder[P[-_, +_]](payload: Http4sPayload[P])
    extends Decoder[Body.Schema[Http4sPayload.Supported[P], *, *], (Option[MediaType], ByteVector)]:
  override def decode[R](
      schema: Body.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[Violations, R] = decodeDetailed(schema, value).leftMap(_.violations)

  /** Retains the error category as well as its structured violations. */
  def decodeDetailed[R](
      schema: Body.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[DecodingFailure, R] = decode(schema.self.self, value)

  private def decode[R](
      body: Body.Value[Http4sPayload.Supported[P], Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[DecodingFailure, R] =
    val (mediaType, bytes) = value

    body match
      case Body.Value.Modify(self, f, _)         => decode(self, value).map(f)
      case Body.Value.Whole(declared, reference) =>
        Http4sBodyDecoder.matches(declared, mediaType).andThen(_ => payload.decode(reference.value, bytes))
      case Body.Value.Binary(declared) => Http4sBodyDecoder.matches(declared, mediaType).map(_ => bytes)

private[http] object Http4sBodyDecoder:
  /** Whether bytes announced as `actual` may be read as `declared`.
    *
    * On `essence`, so that `application/json; charset=utf-8` is `application/json`. The parameters a media type keeps
    * say how bytes became text and where a part ends, which is not what tells two bodies apart.
    */
  private def matches(declared: MediaType, actual: Option[MediaType]): Validated[DecodingFailure, Unit] =
    if actual.forall(_.essence == declared.essence) then ().valid
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.Type(declared.essence.render),
          actual = actual.fold(Data.Null)(mediaType => mediaType.render.asData),
          hint = none
        )
      ).invalid.leftMap(violations => DecodingFailure(Failure.Category.ContentType, violations))
