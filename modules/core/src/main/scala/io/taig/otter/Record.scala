package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant
import cats.data.Validated

abstract class Record[+O <: Data.Optional[Data.Object[?]], A] extends Codec[O, A]:
  self =>

  def fields: Chain[Field[?, ?]]

  final override def modifyDefault(f: Option[A] => Option[A]): Record[O, A] = new Record[O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
      (data, default) match
        case (None, Some(default)) => (data, default).valid
        case _                     => self.decode(data)

  final override def modifyMetadata(f: Metadata => Metadata): Record[O, A] = new Record[O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Record[O, B] = new Record[O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], B)] =
      self.decode(data).map(_.map(f))
    override def encode(b: B): O = self.encode(g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[O, B] = imap(evidence.from)(evidence.to)

  // final def :*[P, B](field: Field[P, B])(using merge: Evidence.Merge[A, B]): Record[O & P, merge.Out] =
  //   zip(field.toRecord)

  // final def *:[P, B](field: Field[P, B])(using merge: Evidence.Merge[B, A]): Record[P & O, merge.Out] =
  //   field.toRecord.zip(this)

  final override def optional: Record[Data.Optional[O], Option[A]] = new Record[Data.Optional[O], Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], Option[A])] =
      ???
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Null           => decode(none)
      case Data.Object(values) => decode(values.some)
      case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)]

object Record:
  extension [O <: Data, A](self: Record[Data.Optional[Data.Object[O]], A])
    def product[P <: Data, B](codec: Record[Data.Optional[Data.Object[P]], B]): Record[Data.Object[O | P], (A, B)] =
      new Record[Data.Object[O | P], (A, B)]:
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[(A, B)] = None
        override def fields: Chain[Field[?, ?]] = self.fields ++ codec.fields
        override def decode(
            values: Option[Chain[(String, Data)]]
        ): Codec.Result[(Option[Chain[(String, Data)]], (A, B))] = self.decode(values) match
          case Validated.Valid((values, a))  => codec.decode(values).map(_.tupleLeft(a))
          case Validated.Invalid(violations) => codec.decode(values).fold(violations.combine, _ => violations).invalid
        override def encode(ab: (A, B)): Data.Object[O | P] = (self.encode(ab._1), codec.encode(ab._2)) match
          case (Data.Object(a), Data.Object(b)) => Data.Object(a ++ b)
          case (a @ Data.Object(_), Data.Null)  => a
          case (Data.Null, b @ Data.Object(_))  => b
          case (Data.Null, Data.Null)           => Data.Object.Empty
    final def zip[P <: Data, B](codec: Record[Data.Optional[Data.Object[P]], B])(using
        merge: Evidence.Merge[A, B]
    ): Record[Data.Object[O | P], merge.Out] = product(codec).imap(merge.apply)(merge.unapply)

  val Empty: Record[Data.Object[Nothing], Unit] = new Record[Data.Object[Nothing], Unit]:
    override def fields: Chain[Field[?, ?]] = Chain.empty
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Unit] = None
    override def decode(
        data: Option[Chain[(String, Data)]]
    ): Codec.Result[(Option[Chain[(String, Data)]], Unit)] = (data, ()).valid
    override def encode(a: Unit): Data.Object[Nothing] = Data.Object.Empty

  def apply[O <: Data, A](field: Field[O, A]): Record[Data.Object[O], A] = new Record[Data.Object[O], A]:
    override def fields: Chain[Field[?, ?]] = Chain.one(field)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
      data
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
        .andThen(field.decode(_).map(_.leftMap(_.some)))
    override def encode(a: A): Data.Object[O] = Data.Object.fromOption(field.encode(a))

  given [O <: Data.Optional[Data.Object[?]]]: Invariant[Record[O, *]] with
    override def imap[A, B](fa: Record[O, A])(f: A => B)(g: B => A): Record[O, B] = fa.imap(f)(g)
