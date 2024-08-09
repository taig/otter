package io.taig.otter.http

import java.nio.charset.Charset
import io.taig.otter.Codec
import cats.syntax.all.*
import cats.data.NonEmptyVector
import io.taig.otter.http.header.MediaType
import io.taig.otter.Violations
import io.taig.otter.XPath
import org.typelevel.ci.*

sealed abstract class Bodies[A]:
  self =>

  def toNev: NonEmptyVector[Body[?]]

  final def orElse[B](bodies: Bodies[B]): Bodies[Either[A, B]] = ???
  // new Bodies[Either[A, B]]:
  //   override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
  //   override def decode(contentType: Option[MediaType], body: Http.Payload): Codec.Result[(MediaType, Either[A, B])] =
  //     ???
  // self
  // .decode(mediaType, body)
  // .map(_.map(_.asLeft))
  // .andThen:
  //   case a @ Some(_) => a.valid
  //   case None        => bodies.decode(mediaType, body).map(_.map(_.asRight))
  // override def encode(charset: Option[Charset], ab: Either[A, B]): (MediaType, Http.Payload) =
  //   ab.fold(self.encode(charset, _), bodies.encode(charset, _))

  final def :+[B](body: Body[B]): Bodies[Either[A, B]] = orElse(body.toBodies)

  final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body.toBodies.orElse(this)

  final def or[B >: A](bodies: Bodies[B]): Bodies[B] = ???

  final def +[B >: A](body: Body[B]): Bodies[B] = or(body.toBodies)

  def decode(contentType: MediaType, body: Http.Payload): Codec.Result[Option[(MediaType, A)]]

  // def encode(a: A): (MediaType, Http.Payload)

object Bodies:
  def apply[A](body: Body[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = NonEmptyVector.one(body)
    override def decode(contentType: MediaType, payload: Http.Payload): Codec.Result[Option[(MediaType, A)]] =
      if body.mediaType.tpe === contentType.tpe
      then
        val charset = contentType.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
        body.decode(charset, payload).tupleLeft(body.mediaType).map(_.some)
      else none.valid
    // override def encode(charset: Option[Charset], a: A): (MediaType, Http.Payload) =
    //     (body.mediaType, body.encode(charset, a))
