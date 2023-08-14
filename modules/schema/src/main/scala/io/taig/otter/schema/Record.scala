package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

import scala.collection.immutable.VectorMap

sealed abstract class Record[A] extends Schema[A]:
  self =>
  override type Self[a] = Record[a]
  override type Codec = OpenApi.Object
  override type Properties[a] = Record.Properties[a]

  def toChain: Chain[Field[?]]

  final def nulls = new Property[Null]:
    override def value: Null = properties.nulls
    override def modify(f: Null => Null): Record[A] = copy(properties.modifyNulls(f))
    def show: Record[A] = apply(Null.Show)
    def hide: Record[A] = apply(Null.Hide)
    def default: Record[A] = apply(Null.Default)

  final override def copy(properties: Record.Properties[A]): Record[A] = new Record[A] with Copy(properties):
    export self.{decodeNone, decodeWithRemainders, encode, toChain}

  final override def optional: Record[Option[A]] = new Record[Option[A]] with Optional:
    export self.toChain
    override def decodeNone: Validated[Violations, Option[A]] = none.valid
    override def decodeWithRemainders(
        remainders: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], Option[A])] =
      self.decodeWithRemainders(remainders).map(_.map(_.some))
    override def encode(a: Option[A], nulls: Null): Option[OpenApi.Object] = a.flatMap(self.encode(_, nulls))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] = new Record[B]
    with Validate[B](validation):
    export self.toChain
    override def decodeNone: Validated[Violations, B] =
      self.decodeNone.andThen(validation(_).leftMap(Violations.root))
    override def decodeWithRemainders(
        remainders: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], B)] =
      self.decodeWithRemainders(remainders).andThen(_.traverse(validation(_).leftMap(Violations.root)))
    override def encode(b: B, nulls: Null): Option[OpenApi.Object] = self.encode(g(b), nulls)

  final def zip[B](record: Record[B]): Record[(A, B)] = new Record[(A, B)]:
    override def toChain: Chain[Field[?]] = self.toChain ++ record.toChain
    override def properties: Record.Properties[(A, B)] = Record.Properties.Default
    override def constraints: Chain[Constraint] = self.constraints ++ record.constraints
    override def isOptional: Boolean = self.isOptional && record.isOptional
    override def decodeNone: Validated[Violations, (A, B)] =
      (self.decode(None), record.decode(None)).tupled
    override def decodeWithRemainders(
        remainders: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], (A, B))] = self.decodeWithRemainders(remainders) match
      case Validated.Valid((remainders, a)) => record.decodeWithRemainders(remainders).map(_.tupleLeft(a))
      case Validated.Invalid(left) =>
        record.decodeWithRemainders(remainders) match
          case Validated.Valid(_)       => left.invalid
          case Validated.Invalid(right) => (left |+| right).invalid
    override def encode(ab: (A, B), nulls: Null): Option[OpenApi.Object] =
      (self.encode(ab._1), record.encode(ab._2)).mapN(_ ++ _)

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi match
    case Some(openapi: OpenApi.Object) => decodeWithRemainders(openapi.toMap).map(_._2)
    case Some(openapi)                 => Violations.rootNec(Violation.tpe("object", actual = openapi.tpe)).invalid
    case None                          => decodeNone
  protected def decodeNone: Validated[Violations, A]
  protected def decodeWithRemainders(
      remainders: VectorMap[String, OpenApi]
  ): Validated[Violations, (VectorMap[String, OpenApi], A)]

  final override def encode(a: A): Option[OpenApi.Object] = encode(a, properties.nulls)
  protected def encode(a: A, nulls: Null): Option[OpenApi.Object]

object Record extends ToRecordOps:
  final case class Properties[+A](description: Option[String], example: Option[A], nulls: Null)
      extends Schema.Properties[A]:
    override type Self[a] = Record.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Record.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Record.Properties[B] = copy(example = f(example))
    def modifyNulls(f: Null => Null): Record.Properties[A] = copy(nulls = f(nulls))
    override def flatMap[B](f: A => Option[B]): Record.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Record.Properties[Nothing] = Properties(None, None, Null.Default)

  val Empty: Record[Unit] = new Record[Unit]:
    override def toChain: Chain[Field[?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def properties: Record.Properties[Unit] = Properties.Default
    override def encode(a: Unit, nulls: Null): Option[OpenApi.Object] = OpenApi.Object.Empty.some
    override def decodeNone: Validated[Violations, Unit] =
      Violations.rootNec(Violation.tpe("object", actual = "null")).invalid
    override def decodeWithRemainders(
        remainders: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], Unit)] = (remainders, ()).valid

  def apply[A](field: => Field[A]): Record[A] = new Record[A]:
    override def toChain: Chain[Field[?]] = Chain.one(field)
    override def properties: Record.Properties[A] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeNone: Validated[Violations, A] =
      Violations.oneNec(History.Root / field.key, Violation.required).invalid
    override def decodeWithRemainders(
        remainders: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], A)] =
      field.decodeWithRemainders(remainders).leftMap(_.modifyHistory(field.key /: _))
    override def encode(b: A, nulls: Null): Option[OpenApi.Object] = field.encode(b, nulls).some
