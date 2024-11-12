package io.taig.otter

import cats.data.NonEmptyChain
import cats.data.NonEmptyChainImpl.Type
import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Union[+O <: Data, A] extends Codec[O, A]:
  self =>

  def codecs: NonEmptyChain[Codec[?, ?]]

  final def discriminator: Option[Discriminator] = metadata.get(???)

  final override def modifyMetadata(f: Metadata => Metadata): Union[O, A] = new Union[O, A]:
    export self.{codecs, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Union[O, A] = new Union[O, A]:
    export self.{codecs, decode, encode, metadata}
    override def default: Option[A] = f(self.default)

  final override def imap[B](f: A => B)(g: B => A): Union[O, B] = new Union[O, B]:
    export self.{codecs, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Union[O, B] = imap(convert.to)(convert.from)

  final def orElse[P <: Data, B](codec: Union[P, B]): Union[O | P, Either[A, B]] = new Union[O | P, Either[A, B]]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = self.codecs ++ codec.codecs
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Either[A, B]] = none
    override def decode(data: Data): Codec.Result[Either[A, B]] =
      self.decode(data).map(_.asLeft).orElse(codec.decode(data).map(_.asRight))
    override def encode(ab: Either[A, B]): O | P = ab match
      case Left(a)  => self.encode(a)
      case Right(b) => codec.encode(b)

  final def :+[P <: Data, B](codec: Union[P, B]): Union[O | P, Either[A, B]] = orElse(codec)

  final def +:[P <: Data, B](codec: Union[P, B]): Union[P | O, Either[B, A]] = codec.toUnion.orElse(self)

object Union:
  extension [O <: Data, A <: Matchable](self: Union[O, A])
    inline def or[P <: Data, B <: Matchable](codec: Union[P, B]): Union[O | P, A | B] = self
      .orElse(codec)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => a.asLeft
        case b: B => b.asRight
      }

    inline def |[P <: Data, B <: Matchable](codec: Codec[P, B]): Union[O | P, A | B] = or(codec.toUnion)

  def apply[O <: Data, A](codec: Codec[O, A]): Union[O, A] = new Union[O, A]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = NonEmptyChain.one(codec)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = none
    override def decode(data: Data): Codec.Result[A] = codec.decode(data)
    override def encode(a: A): O = codec.encode(a)
