package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Body
import scodec.bits.ByteVector

/** Interpreters covering every payload in `P`.
  *
  * Earlier versions accepted a broad endpoint node and discovered a missing interpreter only while handling a request.
  * The requirement parameter now stays on each body, endpoint and route, so serving and calling reject an incomplete
  * registry during construction. Runtime recognition remains confined to this registry because payload alternatives are
  * existential after a schema is stored in a union.
  */
sealed abstract class Http4sPayload[-P[-w, +r]]:
  private[http] def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]]

  private[http] def encode[W](payload: Any, value: W): Option[Either[String, ByteVector]]

  /** The first interpreter recognizing the schema owns both successes and failures. */
  final def orElse[Q[-w, +r]](that: Http4sPayload[Q]): Http4sPayload[Body.Or[P, Q]] =
    new Http4sPayload[Body.Or[P, Q]]:
      override private[http] def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] =
        Http4sPayload.this.decode(payload, bytes).orElse(that.decode(payload, bytes))

      override private[http] def encode[W](payload: Any, value: W): Option[Either[String, ByteVector]] =
        Http4sPayload.this.encode(payload, value).orElse(that.encode(payload, value))

object Http4sPayload:
  /** The shapes the buffered http4s backend can carry with these payload interpreters. */
  type Supported[P[-w, +r]] = Body.Or[Body.Whole[P], Body.Opaque]

  /** A codec must implement its entire advertised alphabet. Failure to encode is distinct from not recognizing it. */
  trait Codec[-P[-w, +r]]:
    def decode[R](payload: P[Nothing, R], bytes: ByteVector): Validated[Violations, R]

    def encode[W](payload: P[W, Any], value: W): Either[String, ByteVector]

  /** `recognize` must accept every schema in `P`, preserving its write/read types. The registry only supplies schemas
    * with those types; an alphabet's type test is the single boundary where their erasure is recovered.
    */
  def apply[P[-w, +r]](recognize: [w, r] => Any => Option[P[w, r]])(codec: Http4sPayload.Codec[P]): Http4sPayload[P] =
    new Http4sPayload[P]:
      override private[http] def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] =
        recognize[Nothing, R](payload).map(codec.decode(_, bytes))

      override private[http] def encode[W](payload: Any, value: W): Option[Either[String, ByteVector]] =
        recognize[W, Any](payload).map(codec.encode(_, value))

  val Empty: Http4sPayload[Nothing] = new Http4sPayload[Nothing]:
    override private[http] def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] = None

    override private[http] def encode[W](payload: Any, value: W): Option[Either[String, ByteVector]] = None
