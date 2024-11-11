package io.taig.otter

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.Data.Required

sealed abstract class Record[+O <: Data, A] extends Codec[Data.Object[O], A]:
  self =>

  def fields: Chain[Field[?, ?]]

  final override def modifyMetadata(f: Metadata => Metadata): Record[O, A] = new Record[O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record[O, A] = new Record[O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[(String, Data)]]): (Option[Vector[(String, Data)]], Codec.Result[A]) =
      (data, default) match
        case (None, Some(default)) => (data, default.valid)
        case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record[O, B] = new Record[O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[(String, Data)]]): (Option[Vector[(String, Data)]], Codec.Result[B]) =
      self.decode(data).map(_.map(f))
    override def encode(b: B): Data.Object[O] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Record[O, B] = imap(convert.to)(convert.from)

  final def zip[P <: Data, B](codec: Record[P, B]): Record[O | P, (A, B)] = new Record[O | P, (A, B)]:
    override def fields: Chain[Field[?, ?]] = self.fields ++ codec.fields
    override def default: Option[(A, B)] = none
    override def metadata: Metadata = Metadata.Empty
    override def decode(
        values: Option[Vector[(String, Data)]]
    ): (Option[Vector[(String, Data)]], Codec.Result[(A, B)]) =
      self.decode(values, treatMissingFieldsAsOptional = ???) match
        case (values, Validated.Valid(a)) =>
          codec.decode(values, treatMissingFieldsAsOptional = ???).map(_.tupleLeft(a))
        case (values, Validated.Invalid(left)) =>
          codec.decode(values, treatMissingFieldsAsOptional = ???) match
            case (values, Validated.Valid(_))       => (values, left.invalid)
            case (values, Validated.Invalid(right)) => (values, (left |+| right).invalid)
    override def encode(ab: (A, B)): Data.Object[O | P] = self.encode(ab._1) ++ codec.encode(ab._2)

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Merge[A, B]): Record[O | P, merge.Out] =
    zip(field.toRecord).imap(merge.apply)(merge.unapply)

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Merge[B, A]): Record[P | O, merge.Out] =
    field.toRecord.zip(this).imap(merge.apply)(merge.unapply)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Object(values) => decode(values.some)._2
    // case Data.Null if isNullable => decode(none)._2
    case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid

  final def decode(
      values: Option[Vector[(String, Data)]],
      treatMissingFieldsAsOptional: Boolean
  ): (Option[Vector[(String, Data)]], Codec.Result[A]) = (values, treatMissingFieldsAsOptional) match
    case (Some(values), true) =>
      val keys = values.map { case (key, _) => key }.toSet
      val fieldsMissing = fields.map(_.name).forall(!keys.contains(_))
      if fieldsMissing then decode(none).leftMap(_ => values.some) else decode(values.some)
    case _ => decode(values)

  def decode(values: Option[Vector[(String, Data)]]): (Option[Vector[(String, Data)]], Codec.Result[A])

  override def encode(a: A): Data.Object[O]

object Record:
  final private case class Apply[O <: Data, A](field: Field[O, A]) extends Record[O, A]:
    override def fields: Chain[Field[?, ?]] = Chain.one(field)
    override def default: Option[A] = none
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Option[Vector[(String, Data)]]): (Option[Vector[(String, Data)]], Codec.Result[A]) =
      values.toValid(Violations.rootNec(Violation.tpe("object", actual = "null"))) match
        case Validated.Valid(values)       => field.decode(values).leftMap(_.some)
        case Validated.Invalid(violations) => (values, violations.invalid)
    override def encode(a: A): Data.Object[O] = Data.Object(field.encode(a))

  def apply[O <: Data, A](field: Field[O, A]): Record[O, A] = Apply(field)

  given [O <: Data]: CodecInvariant[Record[O, *]] with
    override def imap[A, B](fa: Record[O, A])(f: A => B)(g: B => A): Record[O, B] = fa.imap(f)(g)

  given [O <: Data, A]: Metadata.Ops[Record[O, A]] with
    extension (self: Record[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record[O, A] = self.modifyMetadata(f)
