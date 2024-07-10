package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

import io.taig.otter.validation.Violations
import io.taig.otter.Primitive.Reader
import io.taig.otter.validation.Violation

sealed trait Primitive[A] extends Value[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  self =>

  override def imap[C](f: A => C)(g: C => A): Primitive[C] = new Primitive[C]:
    export self.tpe
    override def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[C] =
      self.decodePrimitive(data).map(f)
    override def encode(c: C): Option[Data.Primitive] = self.encode(g(c))

  override def default(value: A): Primitive[A] = new Primitive[A]:
    export self.{encode, tpe}
    override def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[A] =
      data.fold(value.valid)(_ => self.decodePrimitive(data))

  override def optional: Primitive[Option[A]] = new Primitive[Option[A]]:
    export self.tpe
    override def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[Option[A]] =
      data.fold(none.valid)(_ => self.decodePrimitive(data).map(_.some))
    override def encode(a: Option[A]): Option[Data.Primitive] = a.flatMap(self.encode)

object Primitive:
  sealed trait Required[A] extends Primitive[A], Primitive.Required.Reader[A], Primitive.Required.Writer[A]:
    self =>

    override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = new Primitive.Required[C]:
      export self.tpe
      override def decodeRequired(data: Data.Primitive): Codec.Result[C] = self.decodeRequired(data).map(f)
      override def encodeRequired(c: C): Data.Primitive = self.encodeRequired(g(c))

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      self =>

      override def map[C](f: A => C): Primitive.Required.Reader[C] = new Primitive.Required.Reader[C]:
        export self.tpe
        override def decodeRequired(data: Data.Primitive): Codec.Result[C] = self.decodeRequired(data).map(f)

      final override def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[A] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String("null"))))
        .andThen(decodeRequired)

      def decodeRequired(data: Data.Primitive): Codec.Result[A]

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      self =>

      override def contramap[C](f: C => A): Primitive.Required.Writer[C] = new Primitive.Required.Writer[C]:
        export self.tpe
        override def encodeRequired(c: C): Data.Primitive = self.encodeRequired(f(c))

      final override def encode(a: A): Option[Data.Primitive] = Some(encodeRequired(a))
      def encodeRequired(a: A): Data.Primitive

    def apply[A](of: Type[A]): Primitive.Required[A] = new Primitive.Required[A]:
      override def tpe: Type[?] = of
      override def decodeRequired(data: Data.Primitive): Codec.Result[A] = of.decode(data)
      override def encodeRequired(a: A): Data.Primitive = of.encode(a)

  sealed trait Reader[+A] extends Value.Reader[Nothing, Nothing, A]:
    self =>

    def tpe: Type[?]

    override def map[C](f: A => C): Primitive.Reader[C] = new Primitive.Reader[C]:
      export self.tpe
      override def decodePrimitive(data: Option[Data.Primitive]): Validated[Violations[Constraint[Data], Data], C] =
        self.decodePrimitive(data).map(f)

    final override def default[A1 >: A](value: A1): Primitive.Reader[A1] = new Primitive.Reader[A1]:
      export self.tpe
      override def decodePrimitive(data: Option[Data.Primitive]): Validated[Violations[Constraint[Data], Data], A1] =
        data.fold(value.valid)(_ => self.decodePrimitive(data))

    override def optional: Primitive.Reader[Option[A]] = new Primitive.Reader[Option[A]]:
      export self.tpe
      override def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[Option[A]] =
        data.fold(none.valid)(_ => self.decodePrimitive(data).map(_.some))

    final override def decode(data: Option[Data.Value]): Codec.Result[A] = data match
      case Some(data: Data.Primitive) => decodePrimitive(Some(data))
      case Some(data) =>
        Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))).invalid
      case None => decodePrimitive(None)

    def decodePrimitive(data: Option[Data.Primitive]): Codec.Result[A]

  sealed trait Writer[-A] extends Value.Writer[Nothing, Nothing, A]:
    self =>

    def tpe: Type[?]

    override def contramap[C](f: C => A): Primitive.Writer[C] = new Primitive.Writer[C]:
      export self.tpe
      override def encode(c: C): Option[Data.Primitive] = self.encode(f(c))

    override def optional: Primitive.Writer[Option[A]] = new Primitive.Writer[Option[A]]:
      export self.tpe
      override def encode(a: Option[A]): Option[Data.Primitive] = a.flatMap(self.encode)

    override def encode(a: A): Option[Data.Primitive]
