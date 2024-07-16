package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Validation

abstract class Primitive[A] extends Value[Nothing, A]:
  self =>

  override def metadata(f: Metadata => Metadata): Primitive[A] = new Primitive[A]:
    export self.{decode, default, encode, parse, print}
    override def metadata: Metadata = f(self.metadata)

  final override def default(f: Option[A] => Option[A]): Primitive[A] = new Primitive[A]:
    export self.{encode, metadata, print}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)
    override def parse(value: Option[String]): Codec.Result[A] = (value, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.parse(value)

  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)

  def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[B] = new Primitive[B]:
    export self.metadata
    override def default: Option[B] = self.default.flatMap(validation(_).toOption)
    override def decode(data: Data): Codec.Result[B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Data = self.encode(f(b))
    override def parse(value: Option[String]): Codec.Result[B] =
      self.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[String] = self.print(f(b))

  final override def optional: Primitive[Option[A]] = ???

object Primitive:
  abstract class Required[A] extends Primitive[A], Value.Required[Nothing, A]:
    self =>

    final override def metadata(f: Metadata => Metadata): Primitive.Required[A] = new Required[A]:
      export self.{decodeValue, default, encodeValue, parseValue, printValue}
      override def metadata: Metadata = f(self.metadata)

    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = new Required[B]:
      export self.metadata
      override def default: Option[B] = self.default.map(f)
      override def decodeValue(data: Data.Value): Codec.Result[B] = self.decodeValue(data).map(f)
      override def encodeValue(b: B): Data.Value = self.encodeValue(g(b))
      override def parseValue(value: String): Codec.Result[B] = self.parseValue(value).map(f)
      override def printValue(b: B): String = self.printValue(g(b))

  def apply[A](tpe: Type[A]): Primitive.Required[A] = new Required[A]:
    override def metadata: Metadata = Metadata.Empty

    override def default: Option[A] = None

    override def decodeValue(data: Data.Value): Result[A] = data.toPrimitive
      .flatMap(tpe.decode)
      .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))))

    override def encodeValue(a: A): Data.Value = tpe.encode(a)

    override def parseValue(value: String): Codec.Result[A] = tpe
      .parse(value)
      .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(value))))

    override def printValue(a: A): String = tpe.print(a)
