package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

abstract class Dictionary[+O <: Data, A] extends Codec[Data.Object[O], A]:
  self =>

  def constraints: Vector[Constraint.Object]

  def key: Codec[Data.Primitive, ?]

  def codec: Codec[?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Dictionary[O, A] = new Dictionary[O, A]:
    export self.{codec, constraints, decode, encode, key}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Dictionary[O, B] = new Dictionary[O, B]:
    export self.{codec, constraints, key, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Object[O] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Dictionary[O, B] = imap(convert.to)(convert.from)

object Dictionary:
  final private case class Apply[O <: Data, A, B](
      key: Codec[Data.Primitive, A],
      codec: Codec[O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ) extends Dictionary[O, Vector[(A, B)]]:
    override def constraints: Vector[Constraint.Object] =
      minProperties.map(Constraint.Object.MinProperties.apply).toVector ++
        minProperties.map(Constraint.Object.MaxProperties.apply).toVector
    override def metadata: Metadata = Metadata.Empty

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
        .traverse { case (a, b) => (key.parse(a), codec.decode(b)).tupled }
    override def encode(abs: Vector[(A, B)]): Data.Object[O] =
      Data.Object(abs.map { case (a, b) => (key.print(a), codec.encode(b)) })

  final private case class NonEmpty[O <: Data, A, B](
      key: Codec[Data.Primitive, A],
      codec: Codec[O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ) extends Dictionary[O, ((A, B), Vector[(A, B)])]:
    val of = Dictionary(key, codec, minProperties.max(1.some), maxProperties)
    override def constraints: Vector[Constraint.Object] = of.constraints
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[((A, B), Vector[(A, B)])] =
      // Safe to call .head, because `wrapped` will perform a length check
      of.decode(data).map(values => (values.head, values.tail))
    override def encode(abs: ((A, B), Vector[(A, B)])): Data.Object[O] = of.encode(abs._1 +: abs._2)

  def apply[O <: Data, A, B](
      key: Codec[Data.Primitive, A],
      codec: Codec[O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[O, Vector[(A, B)]] = Apply(key, codec, minProperties, maxProperties)

  def nonEmpty[O <: Data, A, B](
      key: Codec[Data.Primitive, A],
      codec: Codec[O, B],
      minProperties: Option[Int],
      maxProperties: Option[Int]
  ): Dictionary[O, ((A, B), Vector[(A, B)])] = NonEmpty(key, codec, minProperties, maxProperties)

  given [O <: Data]: CodecInvariant[Dictionary[O, *]] with
    override def imap[A, B](fa: Dictionary[O, A])(f: A => B)(g: B => A): Dictionary[O, B] =
      fa.imap(f)(g)

  given [O <: Data, A]: Metadata.Ops[Dictionary[O, A]] with
    extension (self: Dictionary[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Dictionary[O, A] = self.modifyMetadata(f)
