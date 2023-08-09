package io.taig.otter.schema

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Product[A] extends Schema[A]:
  self =>
  final override type Self[a] = Product[a]
  final override type Codec = OpenApi.Array
  final override type Properties[a] = Product.Properties[a]

  def toChain: Chain[Schema[?]]

  final override def copy(properties: Product.Properties[A]): Product[A] = new Product[A] with Copy(properties):
    export self.{decode, decodeNone, encode, toChain}

  final override def optional: Product[Option[A]] = new Product[Option[A]] with Optional:
    export self.toChain
    override def decodeNone(index: Int): Validated[Violations, Option[A]] = none.valid
    override def decode(values: Chain[OpenApi], index: Int): Validated[Violations, Option[A]] =
      self.decode(values, index).map(_.some)
    override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] = new Product[B]
    with Validate[B](validation):
    export self.toChain
    override def decodeNone(index: Int): Validated[Violations, B] =
      self.decodeNone(index).andThen(validation(_).leftMap(Violations.root))
    override def decode(values: Chain[OpenApi], index: Int): Validated[Violations, B] =
      self.decode(values, index).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Array] = self.encode(g(b))

  final def prepend[B](schema: => Schema[B]): Product[(B, A)] = new Product[(B, A)]:
    export self.constraints
    override def properties: Product.Properties[(B, A)] = Product.Properties.Default
    override def toChain: Chain[Schema[?]] = schema +: self.toChain
    override def isOptional: Boolean = self.isOptional && schema.isOptional
    override def decodeNone(index: Int): Validated[Violations, (B, A)] =
      (schema.decode(None).leftMap(_.modifyHistory(index /: _)), self.decodeNone(index + 1)).tupled
    override def decode(values: Chain[OpenApi], index: Int): Validated[Violations, (B, A)] = values.uncons match
      case Some((head, tail)) =>
        (schema.decode(head).leftMap(_.modifyHistory(index /: _)), self.decode(tail, index + 1)).tupled
      case None => Violations.oneNec(History.Root / index, Violation.required).invalid
    override def encode(ba: (B, A)): Option[OpenApi.Array] =
      self.encode(ba._2).map(schema.encode(ba._1).getOrElse(OpenApi.Null) +: _)

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi match
    case Some(OpenApi.Array(values)) =>
      val size = values.length
      Validated
        .cond(
          size === 0,
          values,
          Violations.root(
            NonEmptyChain(
              Violation(Constraint.MinItems(size), size.asOpenApi.some),
              Violation(Constraint.MaxItems(size), size.asOpenApi.some)
            )
          )
        )
        .andThen(decode(_, index = 0))
    case Some(openapi) => Violations.rootNec(Violation.tpe("array", openapi.tpe)).invalid
    case None          => decodeNone(index = 0)
  protected def decodeNone(index: Int): Validated[Violations, A]
  protected def decode(values: Chain[OpenApi], index: Int): Validated[Violations, A]

object Product:
  final case class Properties[+A](description: Option[String], example: Option[A]) extends Schema.Properties[A]:
    override type Self[a] = Product.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Product.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Product.Properties[B] = copy(example = f(example))
    override def flatMap[B](f: A => Option[B]): Product.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Product.Properties[Nothing] = Properties(None, None)

  val Empty: Product[Unit] = new Product[Unit]:
    override def properties: Product.Properties[Unit] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toChain: Chain[Schema[?]] = Chain.empty
    override def decodeNone(index: Int): Validated[Violations, Unit] =
      Violations.rootNec(Violation.tpe("array", actual = "null")).invalid
    override def decode(values: Chain[OpenApi], index: Int): Validated[Violations, Unit] = ().valid
    override def encode(a: Unit): Option[OpenApi.Array] = OpenApi.Array.Empty.some

  def apply[A](schema: => Schema[A]): Product[A] = new Product[A]:
    override def properties: Product.Properties[A] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toChain: Chain[Schema[?]] = Chain.one(schema)
    override def decodeNone(index: Int): Validated[Violations, A] =
      Violations.oneNec(History.Root / index, Violation.required).invalid
    override def decode(values: Chain[OpenApi], index: Int): Validated[Violations, A] = values.initLast match
      case Some((_, last)) => schema.decode(last).leftMap(_.modifyHistory(index /: _))
      case None            => Violations.oneNec(History.Root / index, Violation.required).invalid
    override def encode(a: A): Option[OpenApi.Array] = OpenApi.arr(schema.encode(a).getOrElse(OpenApi.Null)).some
