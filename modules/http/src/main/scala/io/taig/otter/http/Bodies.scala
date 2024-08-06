package io.taig.otter.http

import java.nio.charset.Charset
import io.taig.otter.Codec
import cats.syntax.all.*

sealed abstract class Bodies[A]:
  self =>

  def toVector: Vector[Body[?]]

  final def orElse[B](bodies: Bodies[B]): Bodies[Either[A, B]] = new Bodies[Either[A, B]]:
    override def toVector: Vector[Body[?]] = self.toVector ++ bodies.toVector
    override def decode(mediaType: MediaType, body: Http.Payload): Codec.Result[Option[Either[A, B]]] = self
      .decode(mediaType, body)
      .map(_.map(_.asLeft))
      .andThen:
        case a @ Some(_) => a.valid
        case None        => bodies.decode(mediaType, body).map(_.map(_.asRight))
    override def encode(charset: Option[Charset], ab: Either[A, B]): (MediaType, Http.Payload) =
      ab.fold(self.encode(charset, _), bodies.encode(charset, _))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = orElse(body.toBodies)

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body.toBodies.orElse(this)

  def decode(mediaType: MediaType, body: Http.Payload): Codec.Result[Option[A]]

  def encode(charset: Option[Charset], a: A): (MediaType, Http.Payload)

object Bodies:
  def apply[A](body: Body[A]): Bodies[A] = new Bodies[A]:
    override def toVector: Vector[Body[?]] = Vector(body)
    override def decode(mediaType: MediaType, payload: Http.Payload): Codec.Result[Option[A]] =
      if body.mediaType === mediaType
      then body.decode(mediaType.parameters.charset, payload).map(_.some)
      else none.valid
    override def encode(charset: Option[Charset], a: A): (MediaType, Http.Payload) =
      (body.mediaType, body.encode(charset, a))
