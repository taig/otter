package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Record[A](description: Option[String], val nulls: Null) extends Schema[A](description):
  self =>
  final override type Self[a] = Record[a]

  final override def description(f: Option[String] => Option[String]): Record[A] = Record(this, f(description), nulls)
  final def nulls(f: Null => Null): Record[A] = Record(this, description, f(nulls))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

  final override def optional: Record[Option[A]] = new Record[Option[A]](description, nulls):
    export self.constraints
    override def isOptional: Boolean = true
    override def decodeWithRemainders(
        data: Option[Data.Object]
    ): Validated[Violations, (Option[Data.Object], Option[A])] = data match
      case Some(_) => self.decodeWithRemainders(data).map(_.map(_.some))
      case None    => (data, none).valid
    override def encode(a: Option[A], nulls: Null): Option[Data.Object] = a.flatMap(self.encode(_, nulls))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] =
    new Record[B](description, nulls):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decodeWithRemainders(data: Option[Data.Object]): Validated[Violations, (Option[Data.Object], B)] =
        self.decodeWithRemainders(data).andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encode(b: B, nulls: Null): Option[Data.Object] = self.encode(g(b), nulls)

  final def product[B](schema: Record[B]): Record[(A, B)] = ???
  final transparent inline def zip[B](schema: Record[B]): Record[?] = inline (self, schema) match
    case (_: Record[a], _: Record[Unit]) => ??? : Record[a]
    case (_: Record[Unit], _: Record[b]) => ??? : Record[b]
    case (_: Record[a *: as], _: Record[b])    => ??? : Record[Tuple.Append[a *: as, b]]
    case (_: Record[a], _: Record[b])    => ??? : Record[(a, b)]
  final transparent inline def :*[B](field: Field[B]): Record[?] = self.zip(field.toRecord)
  final transparent inline def *:[B](field: Field[B]): Record[?] = field.toRecord.zip(self)

  final override def decode(data: Data): Validated[Violations, A] = data match
    case data: Data.Object => decodeWithRemainders(Some(data)).map(_._2)
    case Data.Null         => decodeWithRemainders(None).map(_._2)
    case _                 => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
  def decodeWithRemainders(data: Option[Data.Object]): Validated[Violations, (Option[Data.Object], A)]
  final override def encode(a: A): Data = encode(a, nulls).getOrElse(Data.Null)
  def encode(a: A, nulls: Null): Option[Data.Object]

object Record:
  def apply[A](schema: Record[A], description: Option[String], nulls: Null): Record[A] =
    new Record[A](description, nulls) { export schema.* }

  val Empty: Record[Unit] = new Record[Unit](None, Null.Default):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeWithRemainders(data: Option[Data.Object]): Validated[Violations, (Option[Data.Object], Unit)] =
      (data, ()).valid
    override def encode(a: Unit, nulls: Null): Option[Data.Object] = Data.Object.Empty.some

  def apply[A](field: Field[A]): Record[A] = new Record[A](None, Null.Default):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeWithRemainders(data: Option[Data.Object]): Validated[Violations, (Option[Data.Object], A)] =
      data match
        case Some(data) => field.decodeWithRemainders(data).map(_.leftMap(_.some))
        case None       => Violations.rootNec(Violation.required).invalid
    override def encode(a: A, nulls: Null): Option[Data.Object] = field.encode(a, nulls).some
