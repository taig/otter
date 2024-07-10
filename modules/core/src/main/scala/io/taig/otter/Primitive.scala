package io.taig.otter

import cats.syntax.all.*

import io.taig.otter.validation.Violations
import io.taig.otter.Primitive.Reader
import io.taig.otter.validation.Violation
import scala.annotation.targetName
import io.taig.otter.Codec.Result

sealed trait Primitive[A] extends Value[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  self =>

  override def asReader: Primitive.Reader[A] = this
  override def asWriter: Primitive.Writer[A] = this

  override def imap[C](f: A => C)(g: C => A): Primitive[C] = ???

  override def default(value: A): Primitive[A] = ???

  final override def optional: Primitive[Option[A]] = ???

object Primitive:
  sealed trait Required[A]
      extends Value.Required[Nothing, Nothing, A],
        Primitive[A],
        Primitive.Required.Reader[A],
        Primitive.Required.Writer[A]:
    self =>

    override def asReader: Primitive.Required.Reader[A] = this
    override def asWriter: Primitive.Required.Writer[A] = this

    override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ???

  object Required:
    sealed trait Reader[+A] extends Value.Required.Reader[Nothing, Nothing, A], Primitive.Reader[A]:
      self =>

      final override def typeName: String = tpe.name

      final override def map[C](f: A => C): Primitive.Required.Reader[C] = ???

      override final def decode(data: Data.Value): Codec.Result[A] = ???
      def decode(data: Data.Primitive): Codec.Result[A]

    sealed trait Writer[-A] extends Value.Required.Writer[Nothing, Nothing, A], Primitive.Writer[A]:
      self =>

      final override def contramap[C](f: C => A): Primitive.Required.Writer[C] = ???

    def apply[A](of: Type[A]): Primitive.Required[A] = new Primitive.Required[A]:
      override def tpe: Type[?] = of

      override def parse(value: String): Codec.Result[A] = of
        .parse(value)
        .toValid(Violations.rootNec(Violation(Constraint.Type(typeName), actual = Data.String(value))))

      override def printRequired(a: A): String = of.print(a)
      override def decode(data: Data.Primitive): Codec.Result[A] = of.decode(data)

      override def encodeRequired(a: A): Data.Primitive = of.encode(a)

  sealed trait Reader[+A] extends Value.Reader[Nothing, Nothing, A]:
    self =>

    def tpe: Type[?]

    override def map[C](f: A => C): Primitive.Reader[C] = new Primitive.Reader[C]:
      export self.tpe
      override def parse(value: Option[String]): Codec.Result[C] = self.parse(value).map(f)
      // override def decode(data: Option[Data.Value]): Codec.Result[C] = self.decode(data).map(f)

    final override def default[A1 >: A](value: A1): Primitive.Reader[A1] = ???

    override def optional: Primitive.Reader[Option[A]] = ???
    override final def decode(data: Option[Data.Value]): Codec.Result[A] = ???
    def decode2(data: Option[Data.Primitive]): Codec.Result[A] = ???

  sealed trait Writer[-A] extends Value.Writer[Nothing, Nothing, A]:
    self =>

    def tpe: Type[?]

    override def contramap[C](f: C => A): Primitive.Writer[C] = new Primitive.Writer[C]:
      export self.tpe
      override def encode(c: C): Option[Data.Primitive] = self.encode(f(c))
      override def print(c: C): Option[String] = self.print(f(c))

    override def optional: Primitive.Writer[Option[A]] = new Primitive.Writer[Option[A]]:
      export self.tpe
      override def encode(a: Option[A]): Option[Data.Primitive] = a.flatMap(self.encode)
      override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

    override def encode(a: A): Option[Data.Primitive]
