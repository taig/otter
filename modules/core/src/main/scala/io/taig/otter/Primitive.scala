package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Validation
import cats.Id as Identity

sealed abstract class Primitive[+F[+a <: Data] <: Data.Optional[a], A] extends Codec[F, Data.Primitive, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A] = new Primitive[F, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Primitive[F, A] = new Primitive[F, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Primitive[F, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[F, B] = new Primitive[F, B]:
    export self.metadata
    override def default: Option[B] = self.default.flatMap(validation(_).toOption)
    override def encode(b: B): F[Data.Primitive] = self.encode(f(b))
    override def decode(data: Data): Codec.Result[B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))

  final override def optional: Primitive[Data.Optional, Option[A]] = new Primitive[Data.Optional, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Primitive] = a.map(self.encode).getOrElse(Data.Null)

object Primitive:
  def apply[A](tpe: Type[A]): Primitive[Identity, A] = new Primitive[Identity, A]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = data.asPrimitive
      .flatMap(tpe.decode)
      .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))))
    override def encode(a: A): Data.Primitive = tpe.encode(a)

  given [F[+a <: Data] <: Data.Optional[a]]: ValidationInvariant[Constraint.Primitive, Primitive[F, *]] with
    extension [A](self: Primitive[F, A])
      override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[F, B] =
        self.ivalidate(validation)(f)

  given [F[+a <: Data] <: Data.Optional[a], A]: Metadata.Ops[Primitive[F, A]] = new Metadata.Ops[Primitive[F, A]]:
    extension (self: Primitive[F, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A] = self.modifyMetadata(f)
