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
  val Payload: Http4sPayload = new Http4sPayload:
    /** The type test recovers the `R` the caller asked for, which is the erasure being crossed. A cast would say the
      * same thing and say it outside a pattern, where the codebase does not allow it; matching to the type the schema
      * is about to be used at keeps the unsoundness where it is visible and `@unchecked` admits to it.
      */
    override def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] =
      payload.asMatchable match
        case json: Json.Node[Nothing, R] @unchecked =>
          Some(Http4sCirce.parse(bytes).andThen(JsonCirceDecoder.decode[R](json, _)))
        case _ => None

    override def encode[W](payload: Any, value: W): Option[ByteVector] =
      payload.asMatchable match
        case json: Json.Node[W, Any] @unchecked =>
          ByteVector.encodeUtf8(JsonCirceEncoder.encode[W](json, value).noSpaces).toOption
        case _ => None

  /** The bytes as a document, or the one violation a document that is not one can produce.
    *
    * Reported as a violation rather than raised, because it is the same kind of fact as a field of the wrong type: the
    * request said it was sending JSON and did not. It reports at the body's own position, which the tier above
    * supplies, so a caller sees `$$.body` and not a bare parse error.
    */
  private def parse(bytes: ByteVector): Validated[Violations, CirceJson] =
    bytes.decodeUtf8.toOption
      .flatMap(io.circe.parser.parse(_).toOption)
      .toValid(
        Violations(
          Violation(constraint = Constraint.Generic.Type("json"), actual = bytes.size.toInt.asData, hint = none)
        )
      )
