package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant
import cats.data.Validated

abstract class Record[+O, A] extends Codec[O, A]:
  self =>

  def fields: Chain[Field[?, ?]]

  final override def modifyDefault(f: Option[A] => Option[A]): Record[O, A] = new Record[O, A]:
    export self.{encodeObject, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
      (data, default) match
        case (None, Some(default)) => (data, default).valid
        case _                     => self.decode(data)

  final override def modifyMetadata(f: Metadata => Metadata): Record[O, A] = new Record[O, A]:
    export self.{decode, default, encodeObject, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Record[O, B] = new Record[O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], B)] =
      self.decode(data).map(_.map(f))
    override def encodeObject(b: B): Option[Data.Object] = self.encodeObject(g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[O, B] = imap(evidence.from)(evidence.to)

  final def product[P, B](codec: Record[P, B]): Record[O & P, (A, B)] = new Record[O & P, (A, B)]:
    override def fields: Chain[Field[?, ?]] = self.fields ++ codec.fields
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(values: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], (A, B))] =
      self.decode(values) match
        case Validated.Valid((values, a))  => codec.decode(values).map(_.tupleLeft(a))
        case Validated.Invalid(violations) => codec.decode(values).fold(violations.combine, _ => violations).invalid
    override def encodeObject(ab: (A, B)): Option[Data.Object] = (
      self.encodeObject(ab._1).getOrElse(Data.Object.Empty) ++
        codec.encodeObject(ab._2).getOrElse(Data.Object.Empty)
    ).some

  final def zip[P, B](codec: Record[P, B])(using merge: Evidence.Merge[A, B]): Record[O & P, merge.Out] = 
    product(codec).imap(merge.apply)(merge.unapply)

  final def :*[P, B](field: Field[P, B])(using merge: Evidence.Merge[A, B]): Record[O & P, merge.Out] =
    zip(field.toRecord)

  final def *:[P, B](field: Field[P, B])(using merge: Evidence.Merge[B, A]): Record[P & O, merge.Out] =
    field.toRecord.zip(this)

  final override def optional: Record[O, Option[A]] = new Record[O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Chain[(String, Data)]]): Result[(Option[Chain[(String, Data)]], Option[A])] = ???
    override def encodeObject(a: Option[A]): Option[Data.Object] = a.flatMap(self.encodeObject)

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Null           => decode(none)
      case Data.Object(values) => decode(values.some)
      case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)]

  final override def encode(a: A): Data = encodeObject(a).getOrElse(Data.Null)

  def encodeObject(a: A): Option[Data.Object]

object Record:
  val Empty: Record[Nothing, Unit] = new Record[Nothing, Unit]:
    override def fields: Chain[Field[?, ?]] = Chain.empty
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Unit] = None
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], Unit)] =
      (data, ()).valid
    override def encodeObject(a: Unit): Option[Data.Object] = Data.Object.Empty.some

  def apply[O, A](field: Field[O, A]): Record[O, A] = new Record[O, A]:
    override def fields: Chain[Field[?, ?]] = Chain.one(field)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
      data
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
        .andThen(field.decode(_).map(_.leftMap(_.some)))
    override def encodeObject(a: A): Option[Data.Object] = Data.Object.fromOption(field.encode(a)).some

  given [O]: Invariant[Record[O, *]] with
    override def imap[A, B](fa: Record[O, A])(f: A => B)(g: B => A): Record[O, B] = fa.imap(f)(g)
