package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import io.taig.otter.Keys.*

sealed abstract class Product[A] extends Codec[A]:
  self =>

  type Of <: Data.Object[?] | Data.Array[?]

  def fields: Fields[?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Product.Of[Optional, Of, A]

  override def modifyDefault(f: Option[A] => Option[A]): Product.Of[Optional, Of, A]

  override def imap[B](f: A => B)(g: B => A): Product.Of[Optional, Of, B]

  override def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Product.Of[Optional, Of, B]

  def to[B](using evidence: Evidence.Product.Aux[B, A]): Product.Of[Optional, Of, B]

  override def optional: Product.Of[Data.Optional, Of, Option[A]]

object Product:
  type Of[F[+a] <: Data.Optional[a], O <: Data.Object[?] | Data.Array[?], A] = Product[A] {
    type Optional[+a] <: F[a]; type Of <: O
  }

sealed abstract class Record[A] extends Product[A]:
  self =>

  final override type Of = Data.Object[Element]

  type Element <: Data

  final def nulls: Attribute[Record.Of[Optional, Element, A], Null] =
    Attribute[Record.Of[Optional, Element, A], Null](this, Keys.nulls, Null.Default)

  final override def modifyMetadata(f: Metadata => Metadata): Record.Of[Optional, Element, A] = new Record[A]:
    export self.{decode, default, encode, encodeSequence, fields, Element, Optional}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record.Of[Optional, Element, A] = new Record[A]:
    export self.{encode, encodeSequence, fields, metadata, Element, Optional}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record.Of[Optional, Element, B] = new Record[B]:
    export self.{fields, metadata, Element, Optional}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B, nulls: Null): self.Out = self.encode(g(b), nulls)
    override def encodeSequence(b: B, nulls: Null): Data.Object[Optional[Element]] = self.encodeSequence(g(b), nulls)

  final override def to[B](using evidence: Evidence.Product.Aux[B, A]): Record.Of[Optional, Element, B] = ???

  final override def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Record.Of[Optional, Element, B] =
    ???

  override def optional: Record.Of[Data.Optional, Element, Option[A]] = new Record[Option[A]]:
    export self.{fields, metadata, Element}
    override type Optional[+a] = Data.Optional[a]
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall { case (_, data) => data === Data.Null } => default.flatten.valid
      case Some(_)                                                                => self.decode(data).map(_.some)
      case None                                                                   => default.flatten.valid
    override def encode(a: Option[A], nulls: Null): Out = a.map(self.encode(_, nulls)).getOrElse(Data.Null)
    override def encodeSequence(a: Option[A], nulls: Null): Data.Object[Optional[Element]] =
      a.fold(Data.Object(self.fields.toVector.map(_.name).tupleRight(Data.Null)))(self.encodeSequence(_, nulls))

  def zip[B](
      codec: Record[B]
  ): Record.Of[Data.Required, self.Optional[self.Element] | codec.Optional[codec.Element], (A, B)] = new Record[(A, B)]:
    override type Optional[+a] = a
    override type Element = self.Optional[self.Element] | codec.Optional[codec.Element]
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def default: Option[(A, B)] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(A, B)] =
      val split = data.map: values =>
        val (left, remainders) = values.filterKeys(self.fields.toVector.map(_.name))
        val (right, _) = remainders.filterKeys(codec.fields.toVector.map(_.name))
        (left, right)
      (self.decode(split.map(_._1)), codec.decode(split.map(_._2))).tupled
    override def encode(ab: (A, B), nulls: Null): Out = encodeSequence(ab, nulls)
    override def encodeSequence(ab: (A, B), nulls: Null): Data.Object[Optional[Element]] =
      self.encodeSequence(ab._1, nulls) ++ codec.encodeSequence(ab._2, nulls)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Object(values) => decode(values.some)
    case Data.Null           => decode(none)
    case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A]

  final override def encode(a: A): self.Out = encode(a, nulls.value)

  def encode(a: A, nulls: Null): self.Out

  protected def encodeSequence(a: A, nulls: Null): Data.Object[Optional[Element]]

object Record:
  type Of[F[+a] <: Data.Optional[a], O <: Data, A] = Record[A] { type Optional[+a] <: F[a]; type Element <: O }

  def apply[O <: Data, A](of: Fields[O, A]): Record.Of[Data.Required, O, A] = new Record[A]:
    override type Optional[+a] = a
    override type Element = O
    override def fields: Fields[?, ?] = of
    override def default: Option[A] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
      .andThen(of.decodeRecord)
    override def encode(a: A, nulls: Null): Data.Object[O] = Data.Object(of.encodeRecord(a, nulls))
    override def encodeSequence(a: A, nulls: Null): Data.Object[Element] = encode(a, nulls)

  given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Record.Of[F, O, A]] with
    extension (self: Record.Of[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record.Of[F, O, A] = self.modifyMetadata(f)

sealed abstract class Tuple[A] extends Product[A]:
  self =>

  final override type Of = Data.Array[Element]

  type Element <: Data

  final override def modifyMetadata(f: Metadata => Metadata): Tuple.Of[Optional, Element, A] = new Tuple[A]:
    export self.{decode, default, encode, encodeSequence, fields, Element, Optional}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple.Of[Optional, Element, A] = new Tuple[A]:
    export self.{encode, encodeSequence, fields, metadata, Element, Optional}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Tuple.Of[Optional, Element, B] = new Tuple[B]:
    export self.{fields, metadata, Element, Optional}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[Data]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Out = self.encode(g(b))
    override def encodeSequence(b: B): Data.Array[Optional[Element]] = self.encodeSequence(g(b))

  override def to[B](using evidence: Evidence.Product.Aux[B, A]): Tuple.Of[Optional, Element, B] = ???

  override def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Tuple.Of[Optional, Element, B] = ???

  final override def optional: Tuple.Of[Data.Optional, Element, Option[A]] = new Tuple[Option[A]]:
    export self.{fields, metadata, Element}
    override type Optional[+a] = Data.Optional[a]
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall(_ === Data.Null) => default.flatten.valid
      case Some(_)                                        => self.decode(data).map(_.some)
      case None                                           => default.flatten.valid
    override def encode(a: Option[A]): Data.Optional[self.Out] = a.map(self.encode).getOrElse(Data.Null)
    override def encodeSequence(a: Option[A]): Data.Array[Optional[Element]] =
      Data.Array.fill(fields.toVector.length)(Data.Null)

  final def zip[B](
      codec: Tuple[B]
  ): Tuple.Of[Data.Required, self.Optional[self.Element] | codec.Optional[codec.Element], (A, B)] = new Tuple[(A, B)]:
    override type Optional[+a] = a
    override type Element = self.Optional[self.Element] | codec.Optional[codec.Element]
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(data: Option[Vector[Data]]): Codec.Result[(A, B)] =
      val split = data.map(_.splitAt(self.fields.toVector.length))

      self.decode(split.map(_._1)) match
        case Validated.Valid(a) => codec.decode(split.map(_._2)).tupleLeft(a)
        case Validated.Invalid(violations) =>
          codec.decode(split.map(_._2)).fold(violations.combine, _ => violations).invalid
    override def encode(ab: (A, B)): Out = encodeSequence(ab)
    override def encodeSequence(ab: (A, B)): Data.Array[Optional[Element]] =
      self.encodeSequence(ab._1) ++ codec.encodeSequence(ab._2)

  override def decode(data: Data): Codec.Result[A] = data match
    case Data.Null => decode(none)
    case Data.Array(values) =>
      val length = self.fields.toVector.length

      if values.length < length
      then
        Violations
          .rootNec(Violation(Constraint.Collection.MinItems(reference = length), actual = Data.Number(values.length)))
          .invalid
      else if values.length > length
      then
        Violations
          .rootNec(Violation(Constraint.Collection.MaxItems(reference = length), actual = Data.Number(values.length)))
          .invalid
      else decode(values.some)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[Data]]): Codec.Result[A]

  protected def encodeSequence(a: A): Data.Array[Optional[Element]]

object Tuple:
  type Of[F[+a] <: Data.Optional[a], O <: Data, A] = Tuple[A] { type Optional[+a] <: F[a]; type Element <: O }

  def apply[O <: Data, A](of: Fields[O, A]): Tuple.Of[Data.Required, O, A] = new Tuple[A]:
    override type Optional[+a] = a
    override type Element = O
    override def fields: Fields[?, ?] = of
    override def default: Option[A] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
      .andThen(of.decodeArray)
    override def encode(a: A): Data.Array[O] = Data.Array(of.encodeArray(a))
    override def encodeSequence(a: A): Data.Array[Element] = encode(a)
