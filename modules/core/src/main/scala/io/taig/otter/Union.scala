package io.taig.otter

import io.taig.otter as Base
import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.data.NonEmptyChainImpl
import cats.Invariant

abstract class Union[+O <: Data, A] extends Codec[O, A]:
  self =>

  def codecs: NonEmptyChain[Codec[?, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Union[O, A] = new Union[O, A]:
    export self.{codecs, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def modifyDefault(f: Option[A] => Option[A]): Union[O, A] = new Union[O, A]:
    export self.{codecs, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Union[O, B] = new Union[O, B]:
    export self.{codecs, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  infix def orElse[P <: Data, B](codec: Union[P, B]): Union[O | P, Either[A, B]] = new Union[O | P, Either[A, B]]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = self.codecs ++ codec.codecs
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Either[A, B]] = None
    override def decode(data: Data): Codec.Result[Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(codec.decode(data).map(_.asRight))
    override def encode(ab: Either[A, B]): O | P = ab.fold(self.encode, codec.encode)

  def :+[P <: Data, B](codec: Codec[P, B]): Union[O | P, Either[A, B]] = ???

  def +:[P <: Data, B](codec: Codec[P, B]): Union[P | O, Either[B, A]] = ??? // codec.toUnion.orElse(self)

  override def optional: Union[Data.Optional[O], Option[A]] = new Union[Data.Optional[O], Option[A]]:
    export self.{codecs, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

object Union:
  def apply[O <: Data, A](of: Codec[O, A]): Union[O, A] = new Union[O, A]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = NonEmptyChain.one(of)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = of.decode(data)
    override def encode(a: A): O = of.encode(a)

  extension [O <: Data, A <: Matchable](self: Union[O, A])
    inline def ||[P <: Data, B <: Matchable](codec: Union[P, B]): Union[O | P, A | B] =
      self
        .orElse(codec)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    inline def |[P <: Data, B <: Matchable](codec: Codec[P, B]): Union[O | P, A | B] =
      (self :+ codec).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  given [O <: Data]: Invariant[Union[O, *]] with
    override def imap[A, B](fa: Union[O, A])(f: A => B)(g: B => A): Union[O, B] = fa.imap(f)(g)
