package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import io.taig.otter.http.codec.Http4sPayload
import io.taig.validation.Violation
import scodec.bits.ByteVector

import scala.compiletime.asMatchable

/** JSON bodies, read and written by io.circe.
  *
  * The one alphabet every API has at least one body in, and the shape any second one takes: recognise the schemas you
  * know by the single type test the erasure leaves possible, answer `None` for everything else, and let
  * [[Http4sPayload.orElse]] put the instances in an order.
  */
object Http4sCirce:
  val Payload: Http4sPayload[Json.Node] = Http4sPayload[Json.Node]([W, R] =>
    (payload: Any) =>
      payload.asMatchable match
        case json: Json.Node[W, R] @unchecked => Some[Json.Node[W, R]](json)
        case _                                => None
  )(new Http4sPayload.Codec[Json.Node]:
    override def decode[R](payload: Json.Node[Nothing, R], bytes: ByteVector): Validated[Violations, R] =
      Http4sCirce.parse(bytes).andThen(JsonCirceDecoder.decode[R](payload, _))

    override def decodeDetailed[R](payload: Json.Node[Nothing, R], bytes: ByteVector): Validated[DecodingFailure, R] =
      Http4sCirce
        .parseDetailed(bytes)
        .andThen(document =>
          JsonCirceDecoder
            .decode[R](payload, document)
            .leftMap(violations => DecodingFailure(Failure.Category.Validation, violations))
        )

    override def encode[W](payload: Json.Node[W, Any], value: W): Either[String, ByteVector] =
      ByteVector.encodeUtf8(JsonCirceEncoder.encode[W](payload, value).noSpaces).leftMap(_.getMessage))

  /** The bytes as a document, or the one violation a document that is not one can produce.
    *
    * Reported as a violation rather than raised, because it is the same kind of fact as a field of the wrong type: the
    * request said it was sending JSON and did not. It reports at the body's own position, which the tier above
    * supplies, so a caller sees `$$.body` and not a bare parse error.
    */
  private def parse(bytes: ByteVector): Validated[Violations, CirceJson] =
    Http4sCirce
      .parseDetailed(bytes)
      .leftMap(_.violations)

  private def parseDetailed(bytes: ByteVector): Validated[DecodingFailure, CirceJson] =
    bytes.decodeUtf8
      .flatMap(io.circe.parser.parse(_))
      .leftMap(cause =>
        DecodingFailure(
          Failure.Category.Syntax,
          Violations(
            Violation(constraint = Constraint.Generic.Type("json"), actual = bytes.size.toInt.asData, hint = none)
          ),
          Some(cause)
        )
      )
      .toValidated
