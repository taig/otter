package io.taig.otter.http

import java.nio.charset.Charset
import io.taig.otter.Codec
import io.taig.otter.http.header.MediaType
import io.taig.otter.Data
import io.taig.otter.Data.Optional

sealed abstract class Body[A]:
  self =>

  def mediaType: MediaType

  def codec: Option[Codec[?, ?, ?]]

  def imap[B](f: A => B)(g: B => A): Body[B]

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = toBodies :+ body

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body +: toBodies

  final def +(body: Body[A]): Bodies[A] = toBodies + body

  final def toBodies: Bodies[A] = Bodies(this)

object Body:
  sealed abstract class Strict[A] extends Body[A]:
    self =>
    final override def imap[B](f: A => B)(g: B => A): Body.Strict[B] = new Strict[B]:
      export self.{codec, mediaType}
      override def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[B] =
        self.decode(charset, payload).map(f)
      override def encode(charset: Option[Charset], b: B): Array[Byte] = self.encode(charset, g(b))

    def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A]

    def encode(charset: Option[Charset], a: A): Array[Byte]

  object Strict:
    def apply[A](
        mediaType: MediaType,
        of: Option[Codec[?, ?, ?]],
        f: (Option[Charset], Array[Byte]) => Codec.Result[A],
        g: (Option[Charset], A) => Array[Byte]
    ): Body.Strict[A] =
      val _mediaType = mediaType

      new Strict[A]:
        override def mediaType: MediaType = _mediaType
        override def codec: Option[Codec[?, ?, ?]] = of
        override def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A] =
          f(charset, payload)
        override def encode(charset: Option[Charset], a: A): Array[Byte] = g(charset, a)

  sealed abstract class Streaming[A] extends Body[A]:
    self =>

    final override def imap[B](f: A => B)(g: B => A): Body.Streaming[B] = new Streaming[B]:
      export self.{codec, mediaType}
      override def encode(charset: Option[Charset]): [F[_]] => Stream[F, B] => Stream[F, Byte] = [F[_]] =>
        (fb: Stream[F, B]) => self.encode(charset = charset)(fb.map(g))
      override def decode(charset: Option[Charset]): [F[_]] => Stream[F, Byte] => Stream[F, B] = [F[_]] =>
        (fa: Stream[F, Byte]) => self.decode(charset)(fa).map(f)

    def decode(charset: Option[Charset]): [F[_]] => Stream[F, Byte] => Stream[F, A]

    def encode(charset: Option[Charset]): [F[_]] => Stream[F, A] => Stream[F, Byte]

  object Streaming:
    def apply[A](
        mediaType: MediaType,
        of: Option[Codec[?, ?, ?]],
        f: [F[_]] => (Option[Charset], Stream[F, Byte]) => Stream[F, A],
        g: [F[_]] => (Option[Charset], Stream[F, A]) => Stream[F, Byte]
    ): Body.Streaming[A] =
      val _mediaType = mediaType

      new Streaming[A]:
        override def mediaType: MediaType = _mediaType
        override def codec: Option[Codec[?, ?, ?]] = of
        override def decode(charset: Option[Charset]): [F[_]] => Stream[F, Byte] => Stream[F, A] = [F[_]] =>
          (fa: Stream[F, Byte]) => f(charset, fa)
        override def encode(charset: Option[Charset]): [F[_]] => Stream[F, A] => Stream[F, Byte] = [F[_]] =>
          (fa: Stream[F, A]) => g(charset, fa)
