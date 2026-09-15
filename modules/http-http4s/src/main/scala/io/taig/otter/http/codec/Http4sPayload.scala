package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Body
import io.taig.otter.http.DecodingFailure
import io.taig.otter.http.Failure
import scodec.bits.ByteVector

/** Interpreters covering every payload in `P`.
  *
  * Total in both directions. A value of `P` is a payload this registry reads and writes, so there is no "not mine" for
  * it to answer and none for a caller to handle. What makes that honest is the requirement parameter every body,
  * endpoint and route carries: serving and calling reject an incomplete registry while they are being built, so an
  * alphabet that reaches here is one an interpreter was supplied for. An earlier version asked `Any` and answered
  * `Option`, and a `None` then became a `500` on the first request that happened to take that branch -- a response
  * describing a gap in the wiring, given to somebody who could do nothing about it.
  *
  * One runtime question survives, and only one: which side of a `P[w, r] | Q[w, r]` a payload is. It is asked where
  * that union is formed and nowhere else. See [[Http4sPayload.Alphabet]].
  */
sealed abstract class Http4sPayload[-P[-_, +_]]:
  private[http] def decode[R](payload: P[Nothing, R], bytes: ByteVector): Validated[DecodingFailure, R]

  private[http] def encode[W](payload: P[W, Any], value: W): Either[String, ByteVector]

object Http4sPayload:
  /** The shapes the buffered http4s backend can carry with these payload interpreters. */
  type Supported[P[-w, +r]] = Body.Or[Body.Whole[P], Body.Opaque]

  /** A codec must implement its entire advertised alphabet. */
  trait Codec[-P[-_, +_]]:
    def decode[R](payload: P[Nothing, R], bytes: ByteVector): Validated[Violations, R]

    /** Override when the parser can distinguish malformed bytes from schema violations. */
    def decodeDetailed[R](payload: P[Nothing, R], bytes: ByteVector): Validated[DecodingFailure, R] =
      decode(payload, bytes).leftMap(violations => DecodingFailure(Failure.Category.Validation, violations))

    def encode[W](payload: P[W, Any], value: W): Either[String, ByteVector]

  /** The one type test an alphabet owes, asked only where two alphabets meet.
    *
    * Phrased as a choice rather than as an `Option`, because the question is never "is this one of mine" on its own: it
    * is always "is this mine, or the other one's", and only a union of the two ever reaches it. The second branch needs
    * no test of its own -- a payload that is not a `P` can only be a `Q` -- which is what leaves no "neither" for
    * anybody to answer, and why [[Http4sPayload.orElse]] composes two total interpreters into a total one.
    *
    * An alphabet's type test is the single place the erasure is crossed. It is a pattern match inside the instance and
    * not a cast, so nothing above this line is written against `Any`.
    */
  trait Alphabet[P[-_, +_]]:
    def select[W, R, Q[-_, +_], A](payload: P[W, R] | Q[W, R])(mine: P[W, R] => A, theirs: Q[W, R] => A): A

    /** Recognising `P` first, and everything it does not recognise as a `Q`. */
    final def orElse[Q[-_, +_]](that: Http4sPayload.Alphabet[Q]): Http4sPayload.Alphabet[Body.Or[P, Q]] =
      new Http4sPayload.Alphabet[Body.Or[P, Q]]:
        override def select[W, R, T[-_, +_], A](payload: (P[W, R] | Q[W, R]) | T[W, R])(
            mine: (P[W, R] | Q[W, R]) => A,
            theirs: T[W, R] => A
        ): A =
          Alphabet.this.select[W, R, Body.Or[Q, T], A](payload)(
            mine(_),
            rest => that.select[W, R, T, A](rest)(mine(_), theirs)
          )

  /** An interpreter, at the alphabet it reads and writes.
    *
    * Invariant where [[Http4sPayload]] is contravariant, because an [[Http4sPayload.Alphabet]] is: its type test both
    * accepts and produces a `P`. Parameters ask for the contravariant supertype, so only the values that compose ever
    * need this name.
    */
  final class Of[P[-_, +_]] private[codec] (
      private[codec] val alphabet: Http4sPayload.Alphabet[P],
      private[codec] val codec: Http4sPayload.Codec[P]
  ) extends Http4sPayload[P]:
    override private[http] def decode[R](payload: P[Nothing, R], bytes: ByteVector): Validated[DecodingFailure, R] =
      codec.decodeDetailed(payload, bytes)

    override private[http] def encode[W](payload: P[W, Any], value: W): Either[String, ByteVector] =
      codec.encode(payload, value)

    /** The first interpreter recognising the schema owns both successes and failures. */
    def orElse[Q[-_, +_]](that: Http4sPayload.Of[Q]): Http4sPayload.Of[Body.Or[P, Q]] =
      new Http4sPayload.Of[Body.Or[P, Q]](
        alphabet.orElse(that.alphabet),
        new Http4sPayload.Codec[Body.Or[P, Q]]:
          override def decode[R](payload: P[Nothing, R] | Q[Nothing, R], bytes: ByteVector): Validated[Violations, R] =
            decodeDetailed(payload, bytes).leftMap(_.violations)

          override def decodeDetailed[R](
              payload: P[Nothing, R] | Q[Nothing, R],
              bytes: ByteVector
          ): Validated[DecodingFailure, R] =
            alphabet.select[Nothing, R, Q, Validated[DecodingFailure, R]](payload)(
              Of.this.decode(_, bytes),
              that.decode(_, bytes)
            )

          override def encode[W](payload: P[W, Any] | Q[W, Any], value: W): Either[String, ByteVector] =
            alphabet.select[W, Any, Q, Either[String, ByteVector]](payload)(
              Of.this.encode(_, value),
              that.encode(_, value)
            )
      )

  def apply[P[-_, +_]](alphabet: Http4sPayload.Alphabet[P])(codec: Http4sPayload.Codec[P]): Http4sPayload.Of[P] =
    new Http4sPayload.Of(alphabet, codec)

  /** Recognising nothing, which is what an API whose bodies are all binary or absent needs.
    *
    * Its type test is total for the reason every other one is: a `Nothing | Q[w, r]` is a `Q[w, r]`, so there is
    * nothing to test and the answer is the other interpreter's.
    */
  val Empty: Http4sPayload.Of[Nothing] = new Http4sPayload.Of[Nothing](
    new Http4sPayload.Alphabet[Nothing]:
      override def select[W, R, Q[-_, +_], A](payload: Nothing | Q[W, R])(
          mine: Nothing => A,
          theirs: Q[W, R] => A
      ): A = theirs(payload)
    ,
    new Http4sPayload.Codec[Nothing]:
      override def decode[R](payload: Nothing, bytes: ByteVector): Validated[Violations, R] = payload
      override def encode[W](payload: Nothing, value: W): Either[String, ByteVector] = payload
  )
