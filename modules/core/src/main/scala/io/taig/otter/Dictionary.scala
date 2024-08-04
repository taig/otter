package io.taig.otter

import cats.syntax.all.*
import cats.Invariant

abstract class Dictionary[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Object[O], A]:
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

  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, O, B] = ???

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Dictionary[F, O, B] =
    imap(evidence.from)(evidence.to)

  final override def optional: Dictionary[Data.Optional, O, Option[A]] = new Dictionary[Data.Optional, O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(self.decode(_).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Object[O]] = a.map(self.encode).getOrElse(Data.Null)

object Dictionary:
  def apply[F[+a] <: Data.Optional[a], O <: Data, A, B](
      key: Codec[Data.Required, Data.Primitive, A],
      of: Codec[F, O, B]
  ): Dictionary[Data.Required, F[O], Vector[(A, B)]] = new Dictionary[Data.Required, F[O], Vector[(A, B)]]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Vector[(A, B)]] = None
    override def decode(data: Data): Codec.Result[Vector[(A, B)]] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .andThen(_.values.traverse { case (a, b) => (key.parseRequired(a), of.decode(b)).tupled })
    override def encode(abs: Vector[(A, B)]): Data.Object[F[O]] =
      Data.Object(abs.map { case (a, b) => (key.printRequired(a), of.encode(b)) })

  given [F[+a] <: Data.Optional[a], O <: Data]: Invariant[Dictionary[F, O, *]] with
    override def imap[A, B](fa: Dictionary[F, O, A])(f: A => B)(g: B => A): Dictionary[F, O, B] =
      fa.imap(f)(g)
