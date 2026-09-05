package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import scodec.bits.ByteVector

/** What the document inside a body is read out of and written into, for whichever alphabets an API's bodies are written
  * in.
  *
  * Open and dispatching at runtime, for the reason [[OpenApiPayload]] is: a body's payload is deliberately unbounded,
  * so by the time an interpreter holds one its alphabet is existential and there is nothing left to dispatch on
  * statically. An alphabet nothing here recognises is reported as an [[io.taig.otter.http.Http4sIssue.Uninterpreted]]
  * rather than thrown.
  *
  * Over `ByteVector` and not over http4s's `EntityDecoder`, which is the decision that keeps this trait usable. An
  * `EntityDecoder[F, A]` would drag `F` into every signature here, and a trait with an effect type cannot be composed
  * with [[Http4sPayload.orElse]] without fixing that type for the whole chain. Bytes cost nothing to speak instead:
  * http4s's own `Entity.Strict` already holds a `ByteVector`, so a whole body crosses without an effect in sight, and
  * only a streamed body -- which is not this trait's business -- ever needs one.
  *
  * The payload is `Any` and the value is not, which is the whole of how the erasure is paid for. An instance recovers
  * `R` -- or `W` -- by the type test it was going to make anyway, so the unsoundness is spent once, inside a pattern
  * match, at the one place that has actually looked at what the payload is. Nothing above this line casts:
  * [[Http4sBodyDecoder]] is handed a `Validated[Violations, R]` already at the type its schema promised.
  */
trait Http4sPayload:
  /** The value this payload reads out of `bytes`, if this is an alphabet it recognises. */
  def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]]

  /** The bytes this payload writes `value` as, if this is an alphabet it recognises. */
  def encode[W](payload: Any, value: W): Option[ByteVector]

  /** This interpreter, falling back to `that` for an alphabet it does not recognise. */
  final def orElse(that: Http4sPayload): Http4sPayload = new Http4sPayload:
    override def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] =
      Http4sPayload.this.decode(payload, bytes).orElse(that.decode(payload, bytes))

    override def encode[W](payload: Any, value: W): Option[ByteVector] =
      Http4sPayload.this.encode(payload, value).orElse(that.encode(payload, value))

object Http4sPayload:
  /** Recognising nothing, which is what an API whose every body is bytes needs. */
  val Empty: Http4sPayload = new Http4sPayload:
    override def decode[R](payload: Any, bytes: ByteVector): Option[Validated[Violations, R]] = None

    override def encode[W](payload: Any, value: W): Option[ByteVector] = None
