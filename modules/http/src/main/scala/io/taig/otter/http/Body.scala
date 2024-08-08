package io.taig.otter.http

import java.nio.charset.Charset
import io.taig.otter.Codec
import cats.syntax.all.*
import io.taig.otter.Data

sealed abstract class Body[A]:
  self =>

  def mediaType: MediaType

  final def imap[B](f: A => B)(g: B => A): Body[B] = new Body[B]:
    export self.mediaType
    override def decode(charset: Option[Charset], body: Http.Payload): Codec.Result[B] =
      self.decode(charset, body).map(f)
    override def encode(charset: Option[Charset], b: B): Http.Payload = self.encode(charset, g(b))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = toBodies :+ body

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body +: toBodies
  
  final def +[B >: A](body: Body[B]): Bodies[B] = toBodies + body

  final def toBodies: Bodies[A] = Bodies(this)

  def decode(charset: Option[Charset], body: Http.Payload): Codec.Result[A]

  def encode(charset: Option[Charset], a: A): Http.Payload

object Body:
  def binary(mediaType: MediaType): Body[Array[Byte]] =
    val _mediaType = mediaType

    new Body[Array[Byte]]:
      override def mediaType: MediaType = _mediaType
      override def decode(charset: Option[Charset], body: Http.Payload): Codec.Result[Array[Byte]] =
        body.data.valid
      override def encode(charset: Option[Charset], a: Array[Byte]): Http.Payload = Http.Payload(a)

  def apply[F[+a] <: Data.Optional[a], O <: Data, A](
      mediaType: MediaType,
      of: Codec[F, O, A],
      f: (Option[Charset], Array[Byte]) => Codec.Result[Data],
      g: (Option[Charset], F[O]) => Array[Byte]
  ): Body[A] =
    val _mediaType = mediaType

    new Body[A]:
      override def mediaType: MediaType = _mediaType
      override def decode(charset: Option[Charset], body: Http.Payload): Codec.Result[A] =
        f(charset, body.data).andThen(of.decode)
      override def encode(charset: Option[Charset], a: A): Http.Payload =
        Http.Payload(g(charset, of.encode(a)))
