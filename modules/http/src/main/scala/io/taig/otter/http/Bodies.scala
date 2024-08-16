package io.taig.otter.http

import cats.syntax.all.*
import cats.data.NonEmptyVector
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.MediaRange
import io.taig.otter.Codec
import org.typelevel.ci.*

sealed abstract class Bodies[A]:
  self =>

  def toNev: NonEmptyVector[Body[?]]

  final def orElse[B](bodies: Bodies[B]): Bodies[Either[A, B]] = new Bodies[Either[A, B]]:
    override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
    override def decode(
        contentType: MediaType,
        body: Array[Byte]
    ): Codec.Result[Option[(MediaType, Either[A, B])]] = self
      .decode(contentType, body)
      .map(_.map(_.map(_.asLeft)))
      .andThen:
        case a @ Some(_) => a.valid
        case None        => bodies.decode(contentType, body).map(_.map(_.map(_.asRight)))
    override def encode(ab: Either[A, B]): (MediaType, Array[Byte]) = ab.fold(self.encode, bodies.encode)
    override def encode(accept: MediaRange, ab: Either[A, B]): Option[(MediaType, Array[Byte])] =
      ab.fold(self.encode(accept, _), bodies.encode(accept, _))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = orElse(body.toBodies)

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body.toBodies.orElse(this)

  final def or(bodies: Bodies[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
    override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]] = self
      .decode(contentType, body)
      .andThen:
        case a @ Some(_) => a.valid
        case None        => bodies.decode(contentType, body)
    override def encode(accept: MediaRange, a: A): Option[(MediaType, Array[Byte])] =
      self.encode(accept, a).orElse(bodies.encode(accept, a))
    override def encode(a: A): (MediaType, Array[Byte]) = bodies.encode(a)

  final def +(body: Body[A]): Bodies[A] = or(body.toBodies)

  def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]]

  final def encode(accept: List[MediaRange], a: A): (MediaType, Array[Byte]) =
    accept.collectFirstSome(encode(_, a)).getOrElse(encode(a))

  def encode(accept: MediaRange, a: A): Option[(MediaType, Array[Byte])]

  def encode(a: A): (MediaType, Array[Byte])

object Bodies:
  def apply[A](body: Body[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = NonEmptyVector.one(body)
    override def decode(contentType: MediaType, payload: Array[Byte]): Codec.Result[Option[(MediaType, A)]] =
      if body.mediaType.tpe === contentType.tpe
      then
        body match
          case body: Body.Strict[?] =>
            val charset = contentType.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
            body.decode(charset, payload).tupleLeft(body.mediaType).map(_.some)
          case _: Body.Streaming[?] => ???
      else none.valid
    override def encode(accept: MediaRange, a: A): Option[(MediaType, Array[Byte])] = ???
    // Option.when(body.mediaType.satisfies(accept)):
    //   // TODO include used charset (if anything other than utf-8) in returned media type?
    //   val charset = accept.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
    //   (body.mediaType, body.encode(charset, a))
    override def encode(a: A): (MediaType, Array[Byte]) = body match
      case body: Body.Strict[?] => (body.mediaType, body.encode(charset = none, a))
      case _: Body.Streaming[?] => ???
