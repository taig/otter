package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Validation
import cats.Id as Identity

abstract class Dictionary[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Object[O], A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, O, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Object[A, B])(f: B => A): Dictionary[F, O, B] =
    new Dictionary[F, O, B]:
      export self.metadata
      override def default: Option[B] = self.default.flatMap(validation(_).toOption)
      override def decode(data: Data): Codec.Result[B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): F[Data.Object[O]] = self.encode(f(b))

  final override def optional: Dictionary[Data.Optional, O, Option[A]] = new Dictionary[Data.Optional, O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(self.decode(_).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Object[O]] = a.map(self.encode).getOrElse(Data.Null)

object Dictionary:
  def apply[F[+a <: Data] <: Data.Optional[a], O <: Data, A, B](
      key: Codec[Identity, Data.Primitive, A],
      value: Codec[F, O, B]
  ): Dictionary[Identity, F[O], Vector[(A, B)]] = new Dictionary:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Vector[(A, B)]] = None
    override def decode(data: Data): Codec.Result[Vector[(A, B)]] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .andThen(_.values.traverse { case (a, b) => (key.parseRequired(a), value.decode(b)).tupled })
    override def encode(abs: Vector[(A, B)]): Data.Object[F[O]] =
      Data.Object(abs.map { case (a, b) => (key.printRequired(a), value.encode(b)) })

  given [F[+a <: Data] <: Data.Optional[a], O <: Data]: Invariant[Dictionary[F, O, *]] with
    override def imap[A, B](fa: Dictionary[F, O, A])(f: A => B)(g: B => A): Dictionary[F, O, B] =
      fa.imap(f)(g)
