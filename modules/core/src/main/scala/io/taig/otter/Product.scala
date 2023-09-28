package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Product[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] = Product[a]

  def toChain: Chain[Schema[?]]

  final override def description(f: Option[String] => Option[String]): Product[A] = Product(this, f(description))

  final override def optional: Product[Option[A]] = new Product[Option[A]](description):
    override def toChain: Chain[Schema[?]] = self.toChain
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, Option[A])] =
      if data.values.forall(_ == Data.Null)
      then (Data.Array.Empty, none).valid
      else self.decodeArrayWithRemainders(data).map(_.map(_.some))
    override def encodeArray(a: Option[A]): Option[Data.Array] = a.flatMap(self.encodeArray)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] = new Product[B](description):
    override def toChain: Chain[Schema[?]] = self.toChain
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, B)] =
      self.decodeArrayWithRemainders(data).andThen(_.traverse(validation(_).leftMap(Violations.root)))
    override def encodeArray(b: B): Option[Data.Array] = self.encodeArray(g(b))

  final def zip[B](schema: Product[B]): Product[(A, B)] = new Product[(A, B)](description):
    override def toChain: Chain[Schema[?]] = self.toChain ++ schema.toChain
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, (A, B))] =
      self.decodeArrayWithRemainders(data).andThen { case (data, a) =>
        schema.decodeArrayWithRemainders(data).map(_.tupleLeft(a))
      }
    override def encodeArray(ab: (A, B)): Option[Data.Array] =
      (self.encodeArray(ab._1), schema.encodeArray(ab._2)) match
        case (Some(a), Some(b)) => Some(a ++ b)
        case (Some(a), None)    => Some(a ++ Data.Array.fill(schema.toChain.length)(Data.Null))
        case (None, Some(b))    => Some(Data.Array.fill(self.toChain.length)(Data.Null) ++ b)
        case (None, None)       => None

  final override def decode(data: Data): Validated[Violations, A] = data match
    case data: Data.Array =>
      val length = toChain.length
      if data.length < length
      then Violations.rootNec(Violation(Constraint.MinItems(length), actual = Data.Number(data.length))).invalid
      else if data.length > length
      then Violations.rootNec(Violation(Constraint.MaxItems(length), actual = Data.Number(data.length))).invalid
      else decodeArrayWithRemainders(data).map(_._2)
    case Data.Null => decodeArrayWithRemainders(Data.Array.fill(toChain.length)(Data.Null)).map(_._2)
    case _         => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
  def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, A)]

  final override def encode(a: A): Data = encodeArray(a).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Data.Array]

object Product:
  def apply[A](schema: Product[A], description: Option[String]): Product[A] =
    new Product[A](description) { export schema.* }

  val Empty: Product[Unit] = new Product[Unit](None):
    override def toChain: Chain[Schema[?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, Unit)] =
      (data, ()).valid
    override def encodeArray(a: Unit): Option[Data.Array] = Data.Array.Empty.some

  def apply[A](schema: Schema[A]): Product[A] = new Product[A](None):
    override def toChain: Chain[Schema[?]] = Chain.one(schema)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, A)] =
      data.values.uncons match
        case Some(head, tail) => schema.decode(head).tupleLeft(Data.Array(tail))
        case None             => Violations.rootNec(Violation.required).invalid
    override def encodeArray(a: A): Option[Data.Array] = Data.Array(Chain.one(schema.encode(a))).some
