package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Body
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
  */
final class Http4sBodyDecoder(payload: Http4sPayload) extends Decoder[Body.Node, (Option[MediaType], ByteVector)]:
  override def decode[R](
      body: Body.Node[Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[Violations, R] = decode(body.self.self, value)

  private def decode[R](
      body: Body.Value[Body.Payload, Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[Violations, R] =
    val (mediaType, bytes) = value

    body match
      case Body.Value.Modify(self, f, _)         => decode(self, value).map(f)
      case Body.Value.Whole(declared, reference) =>
        Http4sBodyDecoder
          .matches(declared, mediaType)
          .andThen: _ =>
            payload.decode[R](reference.value, bytes) match
              case Some(decoded) => decoded
              case None          => Http4sBodyDecoder.uninterpreted(declared)
      case Body.Value.Binary(declared)         => Http4sBodyDecoder.matches(declared, mediaType).map(_ => bytes)
      case Body.Value.Streamed(declared, _, _) => Http4sBodyDecoder.streamed(declared)

object Http4sBodyDecoder:
  /** Whether bytes announced as `actual` may be read as `declared`.
    *
    * On `essence`, so that `application/json; charset=utf-8` is `application/json`. The parameters a media type keeps
    * say how bytes became text and where a part ends, which is not what tells two bodies apart.
    */
  private def matches(declared: MediaType, actual: Option[MediaType]): Validated[Violations, Unit] =
    if actual.forall(_.essence == declared.essence) then ().valid
    else
      Violations(
        Violation(
          constraint = Constraint.Generic.Type(declared.essence.render),
          actual = actual.fold(Data.Null)(mediaType => mediaType.render.asData),
          hint = none
        )
      ).invalid

  private def uninterpreted[R](mediaType: MediaType): Validated[Violations, R] =
    Violations(
      Violation(constraint = Constraint.Generic.Type(mediaType.render), actual = Data.Null, hint = none)
    ).invalid

  private def streamed[R](mediaType: MediaType): Validated[Violations, R] =
    Http4sBodyDecoder.uninterpreted(mediaType)
