package io.taig.otter

import cats.syntax.all.*

import io.taig.otter.validation.Violations
import io.taig.otter.Primitive.Reader
import io.taig.otter.validation.Violation
import scala.annotation.targetName

sealed trait Primitive[A] extends Value[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  self =>

  override def asReader: Primitive.Reader[A] = this
  override def asWriter: Primitive.Writer[A] = this

  override def imap[C](f: A => C)(g: C => A): Primitive[C] = new Primitive[C]:
    export self.tpe
    @targetName("decodePrimitive")
    override def decode(data: Option[Data.Primitive]): Codec.Result[C] = self.asReader.map(f).decode(data)
    override def encode(c: C): Option[Data.Primitive] = self.asWriter.contramap(g).encode(c)

  override def default(value: A): Primitive[A] = new Primitive[A]:
    export self.{encode, tpe}
    @targetName("decodePrimitive")
    override def decode(data: Option[Data.Primitive]): Codec.Result[A] =
      self.asReader.default(value).decode(data)

  override final def optional: Primitive[Option[A]] = new Primitive[Option[A]]:
    export self.tpe
    @targetName("decodePrimitive")
    override def decode(data: Option[Data.Primitive]): Codec.Result[Option[A]] =
      self.asReader.optional.decode(data)
    override def encode(a: Option[A]): Option[Data.Primitive] = self.asWriter.optional.encode(a)

object Primitive:
  sealed trait Required[A] extends Primitive[A], Primitive.Required.Reader[A], Primitive.Required.Writer[A]:
    self =>

    override def asReader: Primitive.Required.Reader[A] = this
    override def asWriter: Primitive.Required.Writer[A] = this

    override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = new Primitive.Required[C]:
      export self.tpe
      override def decode(data: Data.Primitive): Codec.Result[C] = self.asReader.map(f).decode(data)
      override def encodeRequired(c: C): Data.Primitive = self.asWriter.contramap(g).encodeRequired(c)

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      self =>

      override def map[C](f: A => C): Primitive.Required.Reader[C] = new Primitive.Required.Reader[C]:
        export self.tpe
        override def decode(data: Data.Primitive): Codec.Result[C] = self.decode(data).map(f)

      @targetName("decodePrimitive")
      final override def decode(data: Option[Data.Primitive]): Codec.Result[A] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String("null"))))
        .andThen(decode)

      def decode(data: Data.Primitive): Codec.Result[A]

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      self =>

      override def contramap[C](f: C => A): Primitive.Required.Writer[C] = new Primitive.Required.Writer[C]:
        export self.tpe
        override def encodeRequired(c: C): Data.Primitive = self.encodeRequired(f(c))

      final override def encode(a: A): Option[Data.Primitive] = Some(encodeRequired(a))
      def encodeRequired(a: A): Data.Primitive

    def apply[A](of: Type[A]): Primitive.Required[A] = new Primitive.Required[A]:
      override def tpe: Type[?] = of
      override def decode(data: Data.Primitive): Codec.Result[A] = of.decode(data)
      override def encodeRequired(a: A): Data.Primitive = of.encode(a)

  sealed trait Reader[+A] extends Value.Reader[Nothing, Nothing, A]:
    self =>

    def tpe: Type[?]

    override def map[C](f: A => C): Primitive.Reader[C] = new Primitive.Reader[C]:
      export self.tpe
      @targetName("decodePrimitive")
      override def decode(data: Option[Data.Primitive]): Codec.Result[C] = self.decode(data).map(f)

    final override def default[A1 >: A](value: A1): Primitive.Reader[A1] = new Primitive.Reader[A1]:
      export self.tpe
      @targetName("decodePrimitive")
      override def decode(data: Option[Data.Primitive]): Codec.Result[A1] =
        data.fold(value.valid)(_ => self.decode(data))

    override def optional: Primitive.Reader[Option[A]] = new Primitive.Reader[Option[A]]:
      export self.tpe
      @targetName("decodePrimitive")
      override def decode(data: Option[Data.Primitive]): Codec.Result[Option[A]] =
        data.fold(none.valid)(_ => self.decode(data).map(_.some))

    final override def decode(data: Option[Data.Value]): Codec.Result[A] = data match
      case Some(data: Data.Primitive) => decode(Some(data))
      case Some(data) =>
        Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))).invalid
      case None => decode(None)

    @targetName("decodePrimitive")
    def decode(data: Option[Data.Primitive]): Codec.Result[A]

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
