package io.taig.otter.http

import cats.syntax.all.*
import cats.data.NonEmptyVector
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.MediaRange
import io.taig.otter.Codec
import org.typelevel.ci.*
import io.taig.otter.Convert

// TODO allow different codecs via taging, e.g. Bodies[Json[A] | Xml[B] | Csv[C]] (?)
sealed abstract class Bodies[A]:
  self =>

  def toNev: NonEmptyVector[Body[?]]

  final def imap[B](f: A => B)(g: B => A): Bodies[B] = new Bodies[B]:
    export self.toNev
    override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, B)]] =
      self.decode(contentType, body).map(_.map(_.map(f)))
    override def encode(accept: MediaRange, reject: List[MediaRange], b: B): Option[(MediaType, Array[Byte])] =
      self.encode(accept, reject, g(b))
    override def encodeFirst(b: B): (MediaType, Array[Byte]) = self.encodeFirst(g(b))

  final def to[B](convert: Convert[A, B]): Bodies[B] = imap(convert.to)(convert.from)

  final def or(bodies: Bodies[A]): Bodies[A] = new Bodies[A]:
    override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
    override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]] = self
      .decode(contentType, body)
      .andThen:
        case a @ Some(_) => a.valid
        case None        => bodies.decode(contentType, body)
    override def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])] =
      self.encode(accept, reject, a).orElse(bodies.encode(accept, reject, a))
    override def encodeFirst(a: A): (MediaType, Array[Byte]) = bodies.encodeFirst(a)

  final def +(body: Body[A]): Bodies[A] = or(body.toBodies)

  def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]]

  /** Use the first `Body` that matches the given `MediaRange` rules to encode the given `a`
    *
    * @returns
    *   `None` if no `Body` can fullfil the `Accept` rules, otherwise `Some` with the encoded result of the first `Body`
    *   that matches the `Accept` rules
    */
  def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])]

  /** Use the first `Body` to encode the given `a`
    *
    * This method is intented to be used when the client does not submit an `Accept` header or uses a `&ast;&sol;&ast;`
    * wildcard.
    */
  def encodeFirst(a: A): (MediaType, Array[Byte])

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
    override def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])] =
      Option.when(body.mediaType.satisfies(accept) && reject.forall(reject => !body.mediaType.satisfies(reject))):
        val charset = accept.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
        body match
          case body: Body.Strict[?] => (body.mediaType, body.encode(charset, a))
          case _: Body.Streaming[?] => ???

    override def encodeFirst(a: A): (MediaType, Array[Byte]) = body match
      case body: Body.Strict[?] => (body.mediaType, body.encode(charset = none, a))
      case _: Body.Streaming[?] => ???
