package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.Eval

sealed abstract class Branch[+O <: Data, A]:
  self =>

  def name: String

  def codec: Eval[Codec[?, ?]]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Data): Codec.Result[Option[B]] = self.decode(data).map(_.map(f))
    override def encode(b: B): O = self.encode(g(b))

  def to[B](using convert: Convert[A, B]): Branch[O, B] = imap(convert.to)(convert.from)

  final def :+[P <: Data, B](branch: Branch[P, B]): Union[O | P, Either[A, B]] = toUnion :+ branch

  final def +:[P <: Data, B](branch: Branch[P, B]): Union[P | O, Either[B, A]] = branch +: toUnion

  final def toUnion: Union[O, A] = Union(self)

  def decode(data: Data): Codec.Result[Option[A]]

  def encode(a: A): O

object Branch:
  sealed abstract class Tagged[+O <: Data, A] extends Branch[O, A]:
    self =>

    def discriminator: Discriminator

    final override def modifyMetadata(f: Metadata => Metadata): Branch.Tagged[O, A] = new Tagged[O, A]:
      export self.{codec, decode, discriminator, encode, name}
      override def metadata: Metadata = f(self.metadata)

    final override def imap[B](f: A => B)(g: B => A): Branch.Tagged[O, B] = new Tagged[O, B]:
      export self.{codec, discriminator, metadata, name}
      override def decode(data: Data.Object[?]): Codec.Result[Option[B]] = self.decode(data).map(_.map(f))
      override def encode(b: B): O = self.encode(g(b))

    final override def to[B](using convert: Convert[A, B]): Branch.Tagged[O, B] = imap(convert.to)(convert.from)

    final override def decode(data: Data): Codec.Result[Option[A]] = data.asObject
      .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = data.name)))
      .andThen(decode)

    def decode(data: Data.Object[?]): Codec.Result[Option[A]]

  object Tagged:
    final private[otter] case class Apply[O <: Data, A](
        name: String,
        codec: Eval[Codec[O, A]],
        discriminator: Discriminator
    ) extends Branch.Tagged[O, A]:
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Data.Object[?]): Codec.Result[Option[A]] = discriminator match
        case Discriminator.Nested(identifier, value) =>
          data.values
            .collectFirst { case (`identifier`, data) => data }
            .flatMap(_.asPrimitive)
            .map(_.plain)
            .filter(_ === name)
            .flatMap(_ => data.values.collectFirst { case (`value`, data) => data })
            .traverse(codec.value.decode)
        case Discriminator.Merged(identifier) =>
          data.values.collectFirst { case (`identifier`, data) => data } match
            case Some(value) =>
              value.asPrimitive
                .map(_.plain)
                .filter(_ === name)
                .traverse(_ => codec.value.decode(data))
            case None => none.valid
        case Discriminator.Keyed =>
          data.values.collectFirst { case (`name`, data) => data }.traverse(codec.value.decode)
      override def encode(a: A): O = codec.value.encode(a)

  final private[otter] case class Apply[O <: Data, A](name: String, codec: Eval[Codec[O, A]]) extends Branch[O, A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[Option[A]] = codec.value.decode(data).map(_.some)
    override def encode(a: A): O = codec.value.encode(a)

  extension [O <: Data, A <: Matchable](self: Branch[O, A])
    inline def |[P <: Data, B <: Matchable](branch: Branch[P, B]): Union[O | P, A | B] = self.toUnion | branch

  given [O <: Data, A]: Metadata.Ops[Branch[O, A]] with
    extension (self: Branch[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = self.modifyMetadata(f)
