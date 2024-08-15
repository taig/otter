package io.taig.otter.http

import cats.syntax.all.*
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

  // final def :+[B](body: Body[B]): Bodies[Either[A, B]] = toBodies :+ body

  // final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body +: toBodies

  // final def +(body: Body[A]): Bodies[A] = toBodies + body

  final def toBodies: Bodies[A] = Bodies(this)

object Body:
  sealed abstract class Strict[A] extends Body[A]:
    def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A]

    def encode(charset: Option[Charset], a: A): Array[Byte]
  
  sealed abstract class Streaming[A] extends Body[A]:
    def encode[F[_]](charset: Option[Charset], as: Stream[F, A]): Stream[F, Byte]

    def decode[F[_]](charset: Option[Charset], payload: Stream[F, Byte]): Stream[F, A]


  // def apply[A](
  //       mediaType: MediaType,
  //       of: Option[Codec[?, ?, ?]],
  //       f: (Option[Charset], Array[Byte]) => Codec.Result[A],
  //       g: (Option[Charset], A) => Array[Byte]
  //   ): Body[A] =
  //     val _mediaType = mediaType

  //     new Body[A]:
  //       override def mediaType: MediaType = _mediaType
  //       override def codec: Option[Codec[?, ?, ?]] = of
  //       override def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[A] =
  //         f(charset, payload)
  //       override def encode(charset: Option[Charset], a: A): Array[Byte] = g(charset, a)
