package io.taig.otter

import io.taig.enumeration.ext.Mapping
import io.taig.otter.Codec.Result
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Enumeration[+O, A] extends Value[O, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[O, A] = new Enumeration[O, A]:
    export self.{decode, default, encode, parse, print}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Enumeration[O, A] = new Enumeration[O, A]:
    export self.{encode, metadata, print}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)
    override def parse(value: Option[String]): Codec.Result[A] = (value, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.parse(value)

  override def imap[B](f: A => B)(g: B => A): Enumeration[O, B] = new Enumeration[O, B]:
    export self.metadata
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data = self.encode(g(b))
    override def parse(value: Option[String]): Codec.Result[B] = self.parse(value).map(f)
    override def print(b: B): Option[String] = self.print(g(b))

  final override def optional: Enumeration[O, Option[A]] = new Enumeration[O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def parse(value: Option[String]): Codec.Result[Option[A]] =
      value.fold(default.flatten.valid)(_ => self.parse(value).map(_.some))
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

object Enumeration:
  abstract class Required[+O, A] extends Enumeration[O, A], Value.Required[O, A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Enumeration.Required[O, A] = new Required[O, A]:
      export self.{decodeValue, default, encodeValue, parseValue, printValue}
      override def metadata: Metadata = f(self.metadata)

    final override def imap[B](f: A => B)(g: B => A): Enumeration.Required[O, B] = new Required[O, B]:
      export self.metadata
      override def default: Option[B] = self.default.map(f)
      override def decodeValue(data: Data.Value): Codec.Result[B] = self.decodeValue(data).map(f)
      override def encodeValue(b: B): Data.Value = self.encodeValue(g(b))
      override def parseValue(value: String): Codec.Result[B] = self.parseValue(value).map(f)
      override def printValue(b: B): String = self.printValue(g(b))

  def apply[A, B](of: Value.Required[?, A], mapping: Mapping[B, A]): Enumeration.Required[of.type, B] =
    new Enumeration.Required[of.type, B]:
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[B] = None
      override def decodeValue(data: Data.Value): Codec.Result[B] = of
        .decodeValue(data)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid(Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(encode)), actual = data)))
      override def encodeValue(b: B): Data.Value = of.encodeValue(mapping(b))
      override def parseValue(value: String): Codec.Result[B] = of
        .parseValue(value)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid(
              Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(encode)), actual = Data.String(value)))
            )
      override def printValue(b: B): String = of.printValue(mapping(b))
