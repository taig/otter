package io.taig.otter.http

import cats.syntax.all.*
import cats.data.NonEmptyVector
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.Http.Payload
import io.taig.otter.Codec
import io.taig.otter.Violations
import org.typelevel.ci.*

sealed abstract class Bodies[A]:
  self =>

  def toNev: NonEmptyVector[Body[?]]

  final def orElse[B](bodies: Bodies[B]): Bodies[Either[A, B]] = new Bodies[Either[A, B]]:
    override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
    override def decode(contentType: MediaType, body: Http.Payload): Codec.Result[Option[(MediaType, Either[A, B])]] =
      self
        .decode(contentType, body)
        .map(_.map(_.map(_.asLeft)))
        .andThen:
          case a @ Some(_) => a.valid
          case None        => bodies.decode(contentType, body).map(_.map(_.map(_.asRight)))
    override def encode(ab: Either[A, B]): (MediaType, Http.Payload) = ab.fold(self.encode, bodies.encode)
    override def encode(accept: MediaRange, ab: Either[A, B]): Option[(MediaType, Http.Payload)] =
      ab.fold(self.encode(accept, _), bodies.encode(accept, _))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = orElse(body.toBodies)

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body.toBodies.orElse(this)

  final def or(bodies: Bodies[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
    override def decode(contentType: MediaType, body: Http.Payload): Codec.Result[Option[(MediaType, A)]] =
      self
        .decode(contentType, body)
        .andThen:
          case a @ Some(_) => a.valid
          case None        => bodies.decode(contentType, body)
    override def encode(accept: MediaRange, a: A): Option[(MediaType, Http.Payload)] =
      self.encode(accept, a).orElse(bodies.encode(accept, a))
    override def encode(a: A): (MediaType, Http.Payload) = bodies.encode(a)

  final def +(body: Body[A]): Bodies[A] = or(body.toBodies)

  def decode(contentType: MediaType, body: Http.Payload): Codec.Result[Option[(MediaType, A)]]

  final def encode(accept: List[MediaRange], a: A): (MediaType, Http.Payload) =
    accept.collectFirstSome(encode(_, a)).getOrElse(encode(a))

  def encode(accept: MediaRange, a: A): Option[(MediaType, Http.Payload)]

  def encode(a: A): (MediaType, Http.Payload)

object Bodies:
  val Empty: Bodies[Unit] = new Bodies[Unit]:
    override def toNev: NonEmptyVector[Body[?]] = ???
    override def decode(contentType: MediaType, body: Payload): Codec.Result[Option[(MediaType, Unit)]] = ???
    override def encode(accept: MediaRange, a: Unit): Option[(MediaType, Payload)] = ???
    override def encode(a: Unit): (MediaType, Payload) = ???

  def apply[A](body: Body[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = NonEmptyVector.one(body)
    override def decode(contentType: MediaType, payload: Http.Payload): Codec.Result[Option[(MediaType, A)]] =
      if body.mediaType.tpe === contentType.tpe
      then
        val charset = contentType.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
        body.decode(charset, payload).tupleLeft(body.mediaType).map(_.some)
      else none.valid
    override def encode(accept: MediaRange, a: A): Option[(MediaType, Http.Payload)] =
      Option.when(body.mediaType.satisfies(accept)):
        // TODO include used charset (if anything other than utf-8) in returned media type?
        val charset = accept.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
        (body.mediaType, body.encode(charset, a))
    override def encode(a: A): (MediaType, Payload) = (body.mediaType, body.encode(charset = none, a))
