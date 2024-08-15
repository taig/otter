package io.taig.otter.http

import cats.syntax.all.*
import java.nio.charset.Charset
import io.taig.otter.Codec
import io.taig.otter.http.header.MediaType
import io.taig.otter.Data

sealed abstract class Body[A]:
  self =>

  def mediaType: MediaType

  final def imap[B](f: A => B)(g: B => A): Body[B] = new Body[B]:
    export self.mediaType
    override def decode(charset: Option[Charset], body: Array[Byte]): Codec.Result[B] =
      self.decode(charset, body).map(f)
    override def encode(charset: Option[Charset], b: B): Array[Byte] = self.encode(charset, g(b))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = toBodies :+ body

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body +: toBodies

  final def +(body: Body[A]): Bodies[A] = toBodies + body

  final def toBodies: Bodies[A] = Bodies(this)

  def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A]

  def encode(charset: Option[Charset], a: A): Array[Byte]

object Body:
  def binary(mediaType: MediaType): Body[Array[Byte]] =
    val _mediaType = mediaType

    new Body[Array[Byte]]:
      override def mediaType: MediaType = _mediaType
      override def decode(charset: Option[Charset], body: Array[Byte]): Codec.Result[Array[Byte]] =
        body.valid
      override def encode(charset: Option[Charset], a: Array[Byte]): Array[Byte] = a

  def apply[F[+a] <: Data.Optional[a], O <: Data, A](
      mediaType: MediaType,
      of: Codec[F, O, A],
      f: (Option[Charset], Array[Byte]) => Codec.Result[Data],
      g: (Option[Charset], F[O]) => Array[Byte]
  ): Body[A] =
    val _mediaType = mediaType

    new Body[A]:
      override def mediaType: MediaType = _mediaType
      override def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A] =
        f(charset, payload).andThen(of.decode)
      override def encode(charset: Option[Charset], a: A): Array[Byte] = g(charset, of.encode(a))
