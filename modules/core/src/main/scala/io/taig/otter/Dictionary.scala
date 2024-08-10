package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import cats.data.Validated
import io.taig.otter.Data.Required
import io.taig.otter.Codec.Result

abstract class Dictionary[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Object[O], A]:
  self =>

  def constraints: Vector[Constraint.Object]

  final override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{constraints, decode, default, encode, isOptional}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{constraints, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def isOptional: Boolean = default.nonEmpty
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, O, B] = new Dictionary[F, O, B]:
    export self.{constraints, isOptional, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Dictionary[F, O, B] =
    imap(evidence.from)(evidence.to)

  final override def optional: Dictionary[Data.Optional, O, Option[A]] = new Dictionary[Data.Optional, O, Option[A]]:
    export self.{constraints, metadata}
    override def isOptional: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(self.decode(_).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Object[O]] = a.map(self.encode).getOrElse(Data.Null)

object Dictionary:
  def apply[F[+a] <: Data.Optional[a], O <: Data, A, B](
      key: Codec[Data.Required, Data.Primitive, A],
      of: Codec[F, O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[Data.Required, F[O], Vector[(A, B)]] = new Dictionary[Data.Required, F[O], Vector[(A, B)]]:
    override def constraints: Vector[Constraint.Object] =
      minProperties.map(Constraint.Object.MinProperties.apply).toVector ++
        minProperties.map(Constraint.Object.MaxProperties.apply).toVector
    override def isOptional: Boolean = false
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Vector[(A, B)]] = None

    def verifyMinProperties(values: Vector[(String, Data)]): Codec.Result[Unit] = minProperties.traverse_ { reference =>
      val length = values.length
      Validated.cond(
        length >= reference,
        (),
        Violations.rootNec(Violation(Constraint.Object.MinProperties(reference), actual = Data.Number(length)))
      )
    }

    def verifyMaxProperties(values: Vector[(String, Data)]): Codec.Result[Unit] = maxProperties.traverse_ { reference =>
      val length = values.length
      Validated.cond(
        length >= reference,
        (),
        Violations.rootNec(Violation(Constraint.Object.MaxProperties(reference), actual = Data.Number(length)))
      )
    }

    override def decode(data: Data): Codec.Result[Vector[(A, B)]] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .andThen(decode)
    def decode(values: Vector[(String, Data)]): Codec.Result[Vector[(A, B)]] =
      verifyMinProperties(values) *> verifyMaxProperties(values) *> values
        .traverse { case (a, b) => (key.parseRequired(a), of.decode(b)).tupled }
    override def encode(abs: Vector[(A, B)]): Data.Object[F[O]] =
      Data.Object(abs.map { case (a, b) => (key.printRequired(a), of.encode(b)) })

  def nonEmpty[F[+a] <: Data.Optional[a], O <: Data, A, B](
      key: Codec[Data.Required, Data.Primitive, A],
      of: Codec[F, O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[Data.Required, F[O], ((A, B), Vector[(A, B)])] =
    new Dictionary[Data.Required, F[O], ((A, B), Vector[(A, B)])]:
      val wrapped = Dictionary(key, of, minProperties.max(1.some), maxProperties)
      override def constraints: Vector[Constraint.Object] = wrapped.constraints
      override def isOptional: Boolean = false
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[((A, B), Vector[(A, B)])] = None
      override def decode(data: Data): Codec.Result[((A, B), Vector[(A, B)])] =
        // Safe to call .head, because `wrapped` will perform a length check
        wrapped.decode(data).map(values => (values.head, values.tail))
      override def encode(abs: ((A, B), Vector[(A, B)])): Data.Object[F[O]] = wrapped.encode(abs._1 +: abs._2)

  given [F[+a] <: Data.Optional[a], O <: Data]: Invariant[Dictionary[F, O, *]] with
    override def imap[A, B](fa: Dictionary[F, O, A])(f: A => B)(g: B => A): Dictionary[F, O, B] =
      fa.imap(f)(g)
