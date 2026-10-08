package io.taig.otter.http.codec

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.http.Body
import io.taig.otter.http.Frame

/** Element codecs are registered separately from whole-document codecs. */
sealed abstract class Http4sStreams[-P[-_, +_]]:
  private[http] def decode[F[_]: Concurrent, R](
      schema: P[Nothing, R],
      frame: Frame,
      bytes: Stream[F, Byte]
  ): Stream[F, R]
  private[http] def encode[F[_]: Concurrent, W](schema: P[W, Any], frame: Frame, values: Stream[F, W]): Stream[F, Byte]

object Http4sStreams:
  val DefaultMaxFrameBytes: Int = 1048576

  final class Of[P[-_, +_]] private[codec] (
      private[codec] val alphabet: Http4sPayload.Alphabet[P],
      private val delegate: Http4sStreams[P]
  ) extends Http4sStreams[P]:
    override private[http] def decode[F[_]: Concurrent, R](
        schema: P[Nothing, R],
        frame: Frame,
        bytes: Stream[F, Byte]
    ): Stream[F, R] = delegate.decode(schema, frame, bytes)
    override private[http] def encode[F[_]: Concurrent, W](
        schema: P[W, Any],
        frame: Frame,
        values: Stream[F, W]
    ): Stream[F, Byte] = delegate.encode(schema, frame, values)

    def orElse[Q[-_, +_]](that: Http4sStreams.Of[Q]): Http4sStreams.Of[Body.Or[P, Q]] =
      new Http4sStreams.Of(
        alphabet.orElse(that.alphabet),
        new Http4sStreams[Body.Or[P, Q]]:
          override private[http] def decode[F[_]: Concurrent, R](
              schema: P[Nothing, R] | Q[Nothing, R],
              frame: Frame,
              bytes: Stream[F, Byte]
          ): Stream[F, R] =
            alphabet.select[Nothing, R, Q, Stream[F, R]](schema)(
              Of.this.decode(_, frame, bytes),
              that.decode(_, frame, bytes)
            )
          override private[http] def encode[F[_]: Concurrent, W](
              schema: P[W, Any] | Q[W, Any],
              frame: Frame,
              values: Stream[F, W]
          ): Stream[F, Byte] =
            alphabet.select[W, Any, Q, Stream[F, Byte]](schema)(
              Of.this.encode(_, frame, values),
              that.encode(_, frame, values)
            )
      )

  def apply[P[-_, +_]](alphabet: Http4sPayload.Alphabet[P], maxFrameBytes: Int = DefaultMaxFrameBytes)(
      codec: Http4sPayload.Codec[P]
  ): Http4sStreams.Of[P] =
    require(maxFrameBytes > 0, "maxFrameBytes must be positive")
    new Http4sStreams.Of(
      alphabet,
      new Http4sStreams[P]:
        override private[http] def decode[F[_]: Concurrent, R](
            schema: P[Nothing, R],
            frame: Frame,
            bytes: Stream[F, Byte]
        ): Stream[F, R] =
          Http4sFraming
            .decode(frame, maxFrameBytes)(bytes)
            .zipWithIndex
            .evalMap: (bytes, index) =>
              Concurrent[F]
                .catchNonFatal(codec.decodeDetailed(schema, bytes))
                .adaptError { case cause =>
                  val failure = Http4sStreamFailure.syntax(index, "element decoding failed")
                  failure.copy(failure = failure.failure.copy(cause = Some(cause)))
                }
                .flatMap:
                  case Validated.Valid(value)     => value.pure[F]
                  case Validated.Invalid(failure) => Concurrent[F].raiseError(Http4sStreamFailure(index, failure))
        override private[http] def encode[F[_]: Concurrent, W](
            schema: P[W, Any],
            frame: Frame,
            values: Stream[F, W]
        ): Stream[F, Byte] =
          values.zipWithIndex
            .evalMap: (value, index) =>
              Concurrent[F]
                .catchNonFatal(codec.encode(schema, value))
                .adaptError { case cause =>
                  val failure = Http4sStreamFailure.encoding(index, "element encoding failed")
                  failure.copy(failure = failure.failure.copy(cause = Some(cause)))
                }
                .flatMap:
                  case Right(bytes) => Http4sFraming.encode(frame, maxFrameBytes, bytes, index).liftTo[F]
                  case Left(reason) => Concurrent[F].raiseError(Http4sStreamFailure.encoding(index, reason))
            .flatMap(bytes => Stream.chunk(fs2.Chunk.byteVector(bytes)))
    )

  private[http] val Empty: Http4sStreams[Nothing] = new Http4sStreams[Nothing]:
    override private[http] def decode[F[_]: Concurrent, R](
        schema: Nothing,
        frame: Frame,
        bytes: Stream[F, Byte]
    ): Stream[F, R] = schema
    override private[http] def encode[F[_]: Concurrent, W](
        schema: Nothing,
        frame: Frame,
        values: Stream[F, W]
    ): Stream[F, Byte] = schema

  /** A raw byte stream needs no element codec. The uninhabited alphabet records its registration. */
  val Raw: Http4sStreams.Of[Body.Raw] = new Http4sStreams.Of(
    new Http4sPayload.Alphabet[Body.Raw]:
      override def select[W, R, Q[-_, +_], A](
          schema: Body.Raw[W, R] | Q[W, R]
      )(mine: Body.Raw[W, R] => A, theirs: Q[W, R] => A): A =
        import scala.compiletime.asMatchable
        (schema.asMatchable: @unchecked) match
          case other: Q[W, R] @unchecked => theirs(other)
    ,
    new Http4sStreams[Body.Raw]:
      override private[http] def decode[F[_]: Concurrent, R](
          schema: Body.Raw[Nothing, R],
          frame: Frame,
          bytes: Stream[F, Byte]
      ): Stream[F, R] =
        Stream.raiseError(new IllegalStateException("Raw bodies have no element schema"))
      override private[http] def encode[F[_]: Concurrent, W](
          schema: Body.Raw[W, Any],
          frame: Frame,
          values: Stream[F, W]
      ): Stream[F, Byte] =
        Stream.raiseError(new IllegalStateException("Raw bodies have no element schema"))
  )
