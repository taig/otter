package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Validation

sealed abstract class Primitive[+O <: Data.Optional[Data.Primitive], A] extends Codec[O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Primitive[O, A] = new Primitive[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Primitive[O, A] = new Primitive[O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data[?]): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Primitive[O, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[O, B] = new Primitive[O, B]:
    export self.metadata
    override def default: Option[B] = self.default.flatMap(validation(_).toOption)
    override def encode(b: B): O = self.encode(f(b))
    override def decode(data: Data[?]): Codec.Result[B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))

  final override def optional: Primitive[Data.Optional[O], Option[A]] = new Primitive[Data.Optional[O], Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data[?]): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

object Primitive:
  def apply[A](tpe: Type[A]): Primitive[Data.Primitive, A] = new Primitive[Data.Primitive, A]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data[?]): Codec.Result[A] = data.toPrimitive
      .flatMap(tpe.decode)
      .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))))
    override def encode(a: A): Data.Primitive = tpe.encode(a)

  given [O <: Data.Optional[Data.Primitive]]: ValidationInvariant[Constraint.Primitive, Primitive[O, *]] with
    extension [A](self: Primitive[O, A])
      override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[O, B] =
        self.ivalidate(validation)(f)
