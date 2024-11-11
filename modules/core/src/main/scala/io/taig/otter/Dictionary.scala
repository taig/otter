package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

abstract class Dictionary[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Codec[F, Data.Object[O], A]:
  self =>

  def constraints: Vector[Constraint.Object]

  final override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{constraints, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dictionary[F, O, A] = new Dictionary[F, O, A]:
    export self.{constraints, decode, encode, metadata}
    override def default: Option[A] = f(self.default)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, O, B] = new Dictionary[F, O, B]:
    export self.{constraints, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Dictionary[F, O, B] = imap(convert.to)(convert.from)

object Dictionary:
  def apply[F[+a] <: Data.Nullable[a], O <: Data, A, B](
      key: => Codec[Data.Required, Data.Primitive, A],
      of: => Codec[F, O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[Data.Required, F[O], Vector[(A, B)]] = new Dictionary[Data.Required, F[O], Vector[(A, B)]]:
    override def constraints: Vector[Constraint.Object] =
      minProperties.map(Constraint.Object.MinProperties.apply).toVector ++
        minProperties.map(Constraint.Object.MaxProperties.apply).toVector
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
      .andThen(obj => decode(obj.values))
    def decode(values: Vector[(String, Data)]): Codec.Result[Vector[(A, B)]] =
      verifyMinProperties(values) *> verifyMaxProperties(values) *> values
        .traverse { case (a, b) => (key.parseRequired(a), of.decode(b)).tupled }
    override def encode(abs: Vector[(A, B)]): Data.Object[F[O]] =
      Data.Object(abs.map { case (a, b) => (key.printRequired(a), of.encode(b)) })

  def nonEmpty[F[+a] <: Data.Nullable[a], O <: Data, A, B](
      key: => Codec[Data.Required, Data.Primitive, A],
      of: => Codec[F, O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[Data.Required, F[O], ((A, B), Vector[(A, B)])] =
    new Dictionary[Data.Required, F[O], ((A, B), Vector[(A, B)])]:
      val wrapped = Dictionary(key, of, minProperties.max(1.some), maxProperties)
      override def constraints: Vector[Constraint.Object] = wrapped.constraints
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[((A, B), Vector[(A, B)])] = None
      override def decode(data: Data): Codec.Result[((A, B), Vector[(A, B)])] =
        // Safe to call .head, because `wrapped` will perform a length check
        wrapped.decode(data).map(values => (values.head, values.tail))
      override def encode(abs: ((A, B), Vector[(A, B)])): Data.Object[F[O]] = wrapped.encode(abs._1 +: abs._2)

  given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Dictionary[F, O, *]] with
    override def imap[A, B](fa: Dictionary[F, O, A])(f: A => B)(g: B => A): Dictionary[F, O, B] =
      fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Dictionary[F, O, A]] with
    extension (self: Dictionary[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, O, A] = self.modifyMetadata(f)
