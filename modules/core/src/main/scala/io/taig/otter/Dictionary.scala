package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant
import io.taig.otter.validation.Validation

abstract class Dictionary[+O, A] extends Codec[O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dictionary[O, A] = new Dictionary[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dictionary[O, A] = new Dictionary[O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[O, B] =
    ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Object[A, B])(f: B => A): Dictionary[O, B] = new Dictionary[O, B]:
    export self.metadata
    override def default: Option[B] = self.default.flatMap(validation(_).toOption)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Data = self.encode(f(b))

  final override def optional: Dictionary[O, Option[A]] = new Dictionary[O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(self.decode(_).map(_.some))
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)

object Dictionary:
  def apply[A, B](key: Value.Required[?, A], value: Codec[?, B]): Dictionary[value.type, List[(A, B)]] =
    new Dictionary:
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[List[(A, B)]] = None
      override def decode(data: Data): Codec.Result[List[(A, B)]] = data.toObject
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
        .andThen(_.values.toList.traverse { case (a, b) => (key.parseValue(a), value.decode(b)).tupled })
      override def encode(abs: List[(A, B)]): Data =
        Data.Object.fromSeq(abs.map { case (a, b) => (key.printValue(a), value.encode(b)) })

  given [O]: Invariant[Dictionary[O, *]] with
    override def imap[A, B](fa: Dictionary[O, A])(f: A => B)(g: B => A): Dictionary[O, B] =
      fa.imap(f)(g)
