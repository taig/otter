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
    export self.{decodeValues, toChain}

  final override def optional: Product[Option[A]] = new Product[Option[A]] with Optional:
    export self.toChain
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def decodeValues(values: Chain[OpenApi]): Validated[Violations, Option[A]] =
      self.decodeValues(values).map(_.some)
    override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] = new Product[B]
    with Validate[B](validation):
    export self.toChain
    override def decodeValues(values: Chain[OpenApi]): Validated[Violations, B] =
      self.decodeValues(values).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Array] = self.encode(g(b))

  final def zip[B](schema: => Schema[B]): Product[(A, B)] = new Product[(A, B)]:
    export self.constraints
    override def toChain: Chain[Schema[?]] = self.toChain :+ schema
    override def properties: Product.Properties[(A, B)] = Product.Properties.Default
    override def isOptional: Boolean = self.isOptional && schema.isOptional
    override def decodeValues(values: Chain[OpenApi]): Validated[Violations, (A, B)] = values.initLast match
      case Some(init, last) =>
        (self.decodeValues(init), schema.decode(last).leftMap(_.modifyHistory((values.length - 1).toInt /: _))).tupled
      case None => throw new IllegalStateException("Unreachable")
    override def encode(ab: (A, B)): Option[OpenApi.Array] =
      self.encode(ab._1).map(_ :+ schema.encode(ab._2).getOrElse(OpenApi.Null))

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi
    .traverse(openapi => openapi.asArray.toValid(Violations.rootNec(Violation.tpe("array", openapi.tpe))))
    .andThen(decodeArray)
  protected def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, A] = openapi
    .toValid(Violations.rootNec(Violation.required))
    .map(_.toChain)
    .andThen { values =>
      val size = values.length

      Validated.cond(
        size === 0,
        values,
        Violations.root(
          NonEmptyChain(
            Violation(Constraint.MinItems(size), size.asOpenApi.some),
            Violation(Constraint.MaxItems(size), size.asOpenApi.some)
          )
        )
      )
    }
    .andThen(decodeValues)

  protected def decodeValues(values: Chain[OpenApi]): Validated[Violations, A]

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
    override def decodeValues(values: Chain[OpenApi]): Validated[Violations, Unit] = ().valid
    override def encode(a: Unit): Option[OpenApi.Array] = OpenApi.Array.Empty.some

  def apply[A](schema: => Schema[A]): Product[A] = new Product[A]:
    override def properties: Product.Properties[A] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toChain: Chain[Schema[?]] = Chain.one(schema)
    override def decodeValues(values: Chain[OpenApi]): Validated[Violations, A] = values.initLast match
      case Some((_, last)) => schema.decode(last).leftMap(_.modifyHistory(0 /: _))
      case None            => throw new IllegalStateException("Unreachable")
    override def encode(a: A): Option[OpenApi.Array] = OpenApi.arr(schema.encode(a).getOrElse(OpenApi.Null)).some
