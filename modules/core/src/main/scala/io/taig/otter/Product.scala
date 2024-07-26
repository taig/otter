package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated

sealed abstract class Product[+O <: Data, A] extends Codec[O, A]:
  self =>

  def fields: Fields[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Product[O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Product[O, A]
  override def imap[B](f: A => B)(g: B => A): Product[O, B]
  override def optional: Product[Data.Optional[O], Option[A]]

sealed abstract class Record[+O <: Data.Optional[Data.Object[?]], A] extends Product[O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Record[O, A] = ???

  final override def modifyDefault(f: Option[A] => Option[A]): Record[O, A] = ???

  final override def imap[B](f: A => B)(g: B => A): Record[O, B] = ???

  override def optional: Record[Data.Optional[O], Option[A]] = new Record[Data.Optional[O], Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], Option[A])] =
      data.fold(Validated.valid(default.flatten).tupleLeft(data))(_ => self.decode(data).map(_.map(_.some)))
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

  final def product[P <: Data.Optional[Data.Object[?]], B](codec: Record[P, B]): Record[O | P, (A, B)] =
    new Record[O | P, (A, B)]:
      override def fields: Fields[?, ?] = self.fields.product(codec.fields)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[(A, B)] = None
      override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], (A, B))] =
        ???
      override def encode(ab: (A, B)): O | P =
        self.encode(ab._1)

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Object(values) => decode(values.some)
      case Data.Null           => decode(none)
      case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)]

object Record:
  def apply[O <: Data, A](fields: Fields[O, A]): Record[Data.Object[O], A] =
    val _fields = fields

    new Record[Data.Object[O], A]:
      override def fields: Fields[O, A] = _fields
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[A] = None
      override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
        ???
      override def encode(a: A): Data.Object[O] = Data.Object(fields.encode(a))

sealed abstract class Tuple[+O <: Data.Optional[Data.Array[?]], A] extends Product[O, A]:
  final override def modifyMetadata(f: Metadata => Metadata): Tuple[O, A] = ???

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[O, A] = ???

  final override def imap[B](f: A => B)(g: B => A): Tuple[O, B] = ???

  override def optional: Product[Data.Optional[O], Option[A]] = ???

  final def product[P <: Data.Optional[Data.Array[?]], B](codec: Tuple[P, B]): Tuple[O | P, (A, B)] = ???
