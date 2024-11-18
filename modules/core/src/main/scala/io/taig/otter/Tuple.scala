package io.taig.otter

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Tuple[+O <: Data, A] extends Codec[Data.Array[O], A]:
  self =>

  def codecs: Chain[Codec[?, ?]]

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[O, A] = new Tuple[O, A]:
    export self.{codecs, decode, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Tuple[O, B] = new Tuple[O, B]:
    export self.{codecs, metadata}
    override def decode(values: Vector[Data], index: Int): Codec.Result[B] = self.decode(values, index).map(f)
    override def encode(b: B): Data.Array[O] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Tuple[O, B] = imap(convert.to)(convert.from)

  final def zip[P <: Data, B](codec: Tuple[P, B]): Tuple[O | P, (A, B)] = new Tuple[O | P, (A, B)]:
    override def codecs: Chain[Codec[?, ?]] = self.codecs ++ codec.codecs
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[Data], index: Int): Codec.Result[(A, B)] =
      val left = self.codecs.length.toInt
      (
        self.decode(values.slice(0, left), index),
        codec.decode(values.slice(left, values.length), index = index + left)
      ).tupled
    override def encode(ab: (A, B)): Data.Array[O | P] = self.encode(ab._1) ++ codec.encode(ab._2)

  def :*[P <: Data, B](codec: Codec[P, B])(using
      merge: Merge[A, B]
  ): Tuple[O | P, merge.Out] = zip(codec.toTuple).imap(merge.apply)(merge.unapply)

  def *:[P <: Data.Nullable[Data.Array[Data]], B](codec: Codec[P, B])(using
      merge: Merge[B, A]
  ): Tuple[P | O, merge.Out] = codec.toTuple.zip(self).imap(merge.apply)(merge.unapply)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Array(values) =>
      val reference = codecs.length.toInt
      val actual = values.length
      if actual > reference then
        Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference), actual = Data.Number(actual))).invalid
      else if actual < reference then
        Violations.rootNec(Violation(Constraint.Collection.MinItems(reference), actual = Data.Number(actual))).invalid
      else decode(values, index = 0)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  protected def decode(values: Vector[Data], index: Int): Codec.Result[A]

  override def encode(a: A): Data.Array[O]

object Tuple:
  extension [O <: Data.Value, A](self: Tuple[O, A])
    def optional: Tuple[Data.Nullable[O], Option[A]] = new Tuple[Data.Nullable[O], Option[A]]:
      export self.{codecs, metadata}
      override def decode(values: Vector[Data], index: Int): Codec.Result[Option[A]] =
        if values.forall(_.isNull) then none.valid else self.decode(values, index).map(_.some)
      override def encode(a: Option[A]): Data.Array[Data.Nullable[O]] = a match
        case Some(a) => self.encode(a).map(Data.Nullable.Some.apply)
        case None    => Data.Array.fill(codecs.length.toInt)(Data.Null)

  val Empty: Tuple[Nothing, Unit] = new Tuple[Nothing, Unit]:
    override def codecs: Chain[Codec[?, ?]] = Chain.empty
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[Data], index: Int): Codec.Result[Unit] = ().valid
    override def encode(a: Unit): Data.Array[Nothing] = Data.Array.Empty

  final private case class Apply[O <: Data, A](codec: Codec[O, A]) extends Tuple[O, A]:
    override def codecs: Chain[Codec[?, ?]] = Chain.one(codec)
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[Data], index: Int): Codec.Result[A] =
      codec.decode(values.head).leftMap(index /: _)
    override def encode(a: A): Data.Array[O] = Data.Array.one(codec.encode(a))

  def apply[O <: Data, A](codec: Codec[O, A]): Tuple[O, A] = Apply(codec)
