package io.taig.otter

import io.taig.otter as Base
import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.data.NonEmptyChainImpl

abstract class Union[+O, A] extends Codec[O, A]:
  self =>

  def codecs: NonEmptyChain[Codec[?, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Union[O, A] = new Union[O, A]:
    export self.{codecs, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def default(f: Option[A] => Option[A]): Union[O, A] = new Union[O, A]:
    export self.{codecs, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Union[O, B] = new Union[O, B]:
    export self.{codecs, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data = self.encode(g(b))

  def orElse[P, B](union: Union[P, B]): Union[O | P, Either[A, B]] = new Union[O | P, Either[A, B]]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = self.codecs ++ union.codecs
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Either[A, B]] = None
    override def decode(data: Data): Codec.Result[Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(union.decode(data).map(_.asRight))
    override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, union.encode)

  override def optional: Union[O, Option[A]] = new Union[O, Option[A]]:
    export self.{codecs, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)

object Union:
  abstract class Value[+O, A] extends Union[O, A], Base.Value[O, A]:
    self =>

    override def modifyMetadata(f: Metadata => Metadata): Union.Value[O, A] = new Union.Value[O, A]:
      export self.{codecs, decode, default, encode, parse, print}
      override def metadata: Metadata = f(self.metadata)

    final override def default(f: Option[A] => Option[A]): Union.Value[O, A] =
      new Union.Value[O, A]:
        export self.{codecs, encode, metadata, print}
        override def default: Option[A] = f(self.default)
        override def decode(data: Data): Codec.Result[A] = (data, default) match
          case (Data.Null, Some(default)) => default.valid
          case _                          => self.decode(data)
        override def parse(value: Option[String]): Codec.Result[A] = (value, default) match
          case (None, Some(default)) => default.valid
          case _                     => self.parse(value)
    override def imap[B](f: A => B)(g: B => A): Union.Value[O, B] = new Union.Value[O, B]:
      export self.{codecs, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
      override def encode(b: B): Data = self.encode(g(b))
      override def parse(value: Option[String]): Codec.Result[B] = self.parse(value).map(f)
      override def print(b: B): Option[String] = self.print(g(b))

    def orElse[P, B](union: Union.Value[P, B]): Union.Value[O | P, Either[A, B]] = new Union.Value[O | P, Either[A, B]]:
      override def codecs: NonEmptyChain[Codec[?, ?]] = self.codecs ++ union.codecs
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(data: Data): Codec.Result[Either[A, B]] =
        self.decode(data).map(_.asLeft).findValid(union.decode(data).map(_.asRight))
      override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, union.encode)
      override def parse(value: Option[String]): Codec.Result[Either[A, B]] =
        self.parse(value).map(_.asLeft).findValid(union.parse(value).map(_.asRight))
      override def print(ab: Either[A, B]): Option[String] = ab.fold(self.print, union.print)

    final override def optional: Union.Value[O, Option[A]] = new Union.Value[O, Option[A]]:
      export self.{codecs, metadata}
      override def default: Option[Option[A]] = self.default.map(_.some)
      override def decode(data: Data): Codec.Result[Option[A]] =
        data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Codec.Result[Option[A]] =
        value.fold(default.flatten.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  object Value:
    abstract class Required[O, A] extends Union.Value[O, A], Base.Value.Required[O, A]:
      self =>

      override def modifyMetadata(f: Metadata => Metadata): Union.Value.Required[O, A] = new Union.Value.Required[O, A]:
        export self.{codecs, decodeValue, default, encodeValue, parseValue, printValue}
        override def metadata: Metadata = f(self.metadata)

      override def imap[B](f: A => B)(g: B => A): Union.Value.Required[O, B] = new Union.Value.Required[O, B]:
        export self.{codecs, metadata}
        override def default: Option[B] = self.default.map(f)
        override def decodeValue(data: Data.Value): Codec.Result[B] = self.decodeValue(data).map(f)
        override def encodeValue(b: B): Data.Value = self.encodeValue(g(b))
        override def parseValue(value: String): Codec.Result[B] = self.parseValue(value).map(f)
        override def printValue(b: B): String = self.printValue(g(b))

      def orElse[P, B](union: Union.Value.Required[P, B]): Union.Value.Required[O | P, Either[A, B]] =
        new Union.Value.Required[O | P, Either[A, B]]:
          override def codecs: NonEmptyChain[Codec[?, ?]] = self.codecs ++ union.codecs
          override def metadata: Metadata = Metadata.Empty
          override def default: Option[Either[A, B]] = None
          override def decodeValue(data: Data.Value): Codec.Result[Either[A, B]] =
            self.decodeValue(data).map(_.asLeft).findValid(union.decodeValue(data).map(_.asRight))
          override def encodeValue(ab: Either[A, B]): Data.Value = ab.fold(self.encodeValue, union.encodeValue)
          override def parseValue(value: String): Codec.Result[Either[A, B]] =
            self.parseValue(value).map(_.asLeft).findValid(union.parseValue(value).map(_.asRight))
          override def printValue(ab: Either[A, B]): String = ab.fold(self.printValue, union.printValue)

  def apply[A](of: Codec[?, A]): Union[of.type, A] = new Union[of.type, A]:
    override def codecs: NonEmptyChain[Codec[?, ?]] = NonEmptyChain.one(of)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = of.decode(data)
    override def encode(a: A): Data = of.encode(a)
