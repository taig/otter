package io.taig.otter

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*

sealed abstract class Tuple[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Codec[F, Data.Array[O], A]:
  self =>

  def codecs: Chain[Codec[?, ?, ?]]

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{codecs, encode, encodeSequence, metadata}
    override def default: Option[A] = f(self.default)
    override def isNullable: Boolean = default.nonEmpty
    override def decode(values: Option[Vector[Data]], index: Int): Codec.Result[A] = (values, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(values, index)

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{codecs, decode, default, encode, encodeSequence, isNullable}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Tuple[F, O, B] = new Tuple[F, O, B]:
    export self.{codecs, isNullable, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(values: Option[Vector[Data]], index: Int): Codec.Result[B] =
      self.decode(values, index).map(f)
    override def encode(b: B): F[Data.Array[O]] = self.encode(g(b))
    override def encodeSequence(a: B): Data.Array[F[O]] = self.encodeSequence(g(a))

  final override def to[B](using convert: Convert[A, B]): Tuple[F, O, B] = imap(convert.to)(convert.from)

  final override def nullable: Tuple[Data.Nullable, O, Option[A]] = ???

  final def zip[G[+a] <: Data.Nullable[a], P <: Data, B](
      codec: Tuple[G, P, B]
  ): Tuple[Data.Required, F[O] | G[P], (A, B)] = new Tuple[Data.Required, F[O] | G[P], (A, B)]:
    override def codecs: Chain[Codec[?, ?, ?]] = self.codecs ++ codec.codecs
    override def isNullable: Boolean = false
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = none
    override def decode(values: Option[Vector[Data]], index: Int): Codec.Result[(A, B)] = values match
      case Some(values) =>
        val left = self.codecs.length.toInt
        (
          self.decode(values.slice(0, left).some, index),
          codec.decode(values.slice(left, values.length).some, index = index + left)
        ).tupled
      case None => Violations.rootNec(Violation.tpe("array", actual = "null")).invalid
    override def encode(ab: (A, B)): Data.Array[F[O] | G[P]] = encodeSequence(ab)
    override def encodeSequence(ab: (A, B)): Data.Array[F[O] | G[P]] =
      self.encodeSequence(ab._1) ++ codec.encodeSequence(ab._2)

  def :*[G[+a] <: Data.Nullable[a], P <: Data, B](codec: Codec[G, P, B])(using
      merge: Merge[A, B]
  ): Tuple[Data.Required, F[O] | G[P], merge.Out] = zip(codec.toTuple).imap(merge.apply)(merge.unapply)

  def *:[G[+a] <: Data.Nullable[a], P <: Data, B](codec: Codec[G, P, B])(using
      merge: Merge[B, A]
  ): Tuple[Data.Required, G[P] | F[O], merge.Out] = codec.toTuple.zip(self).imap(merge.apply)(merge.unapply)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Array(values) =>
      val reference = codecs.length.toInt
      val actual = values.length
      if actual > reference then
        Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference), actual = Data.Number(actual))).invalid
      else if actual < reference then
        Violations.rootNec(Violation(Constraint.Collection.MinItems(reference), actual = Data.Number(actual))).invalid
      else decode(values.some, index = 0)
    case Data.Null if isNullable => decode(none, index = 0)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  protected def decode(values: Option[Vector[Data]], index: Int): Codec.Result[A]

  override def encode(a: A): F[Data.Array[O]]
  protected def encodeSequence(a: A): Data.Array[F[O]]

object Tuple:
  val Empty: Tuple[Data.Required, Nothing, Unit] = new Tuple[Data.Required, Nothing, Unit]:
    override def codecs: Chain[Codec[?, ?, ?]] = Chain.empty
    override def isNullable: Boolean = false
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Unit] = none
    override def decode(values: Option[Vector[Data]], index: Int): Codec.Result[Unit] = ().valid
    override def encode(a: Unit): Data.Array[Nothing] = Data.Array.Empty
    override def encodeSequence(a: Unit): Data.Array[Nothing] = Data.Array.Empty

  final private case class Apply[+F[+a] <: Data.Nullable[a], +O <: Data, A](codec: Codec[F, O, A])
      extends Tuple[Data.Required, F[O], A]:
    override def codecs: Chain[Codec[?, ?, ?]] = Chain.one(codec)
    override def isNullable: Boolean = false
    override def default: Option[A] = none
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Option[Vector[Data]], index: Int): Codec.Result[A] =
      values.toValid(Violations.rootNec(Violation.tpe("array", actual = "null"))) match
        case Validated.Valid(values)       => codec.decode(values.head).leftMap(index /: _) // TODO validate again?
        case Validated.Invalid(violations) => violations.invalid
    override def encode(a: A): Data.Array[F[O]] = Data.Array.one(codec.encode(a))
    override def encodeSequence(a: A): Data.Array[F[O]] = encode(a)

  def apply[F[+a] <: Data.Nullable[a], O <: Data, A](codec: Codec[F, O, A]): Tuple[Data.Required, F[O], A] =
    Apply(codec)
