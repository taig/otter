package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.{OpenApi, Specification}
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Collection[F[a] <: Schema[a], A] extends Schema[A]:
  self =>
  override type Self[a] = Collection[F, a]

  override def specification: Specification.Array

  final override def modifySpecification(f: Specification.Value => Specification.Value): Collection[F, A] =
    ???

  override def optional: Collection[F, Option[A]] = new Collection[F, Option[A]] with Optional:
    export self.specification
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)
    override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Option[A]] =
      self.parse(values).map(_.some)
    override def print(a: Option[A]): Option[Chain[Option[String]]] = a.flatMap(self.print)

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[F, B] = new Collection[F, B]
    with Validate[B](validation):
    export self.specification
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, B] =
      self.decodeArray(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Array] = self.encode(g(b))
    override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, B] =
      self.parse(values).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[Chain[Option[String]]] = self.print(g(b))

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi
    .traverse(openapi => openapi.asArray.toValid(Violations.rootNec(Violation.tpe("array", openapi.tpe))))
    .andThen(decodeArray)
  protected def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, A]
  override def encode(a: A): Option[OpenApi.Array]

  protected def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A]
  protected def print(as: A): Option[Chain[Option[String]]]

object Collection:
  extension [A](self: Collection[Schema.Value, A])
    def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A] = self.parse(values)
    def print(as: A): Option[Chain[Option[String]]] = self.print(as)

  def apply[F[a] <: Schema[a], A](of: => F[A]): Collection[F, Chain[A]] = new Collection[F, Chain[A]]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Chain[A]] = openapi
      .toValid(Violations.rootNec(Violation.required))
      .andThen: openapi =>
        openapi.toChain.zipWithIndex.traverse { case (value, index) =>
          of.decode(value).leftMap(_.modifyHistory(index /: _))
        }
    override def encode(as: Chain[A]): Option[OpenApi.Array] =
      OpenApi.Array(as.map(of.encode(_).getOrElse(OpenApi.Null))).some
    override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Chain[A]] = of match
      case schema: Schema.Value[A] =>
        values
          .toValid(Violations.rootNec(Violation.required))
          .andThen: values =>
            values.zipWithIndex.traverse { case (value, index) =>
              schema.parse(value).leftMap(_.modifyHistory(index /: _))
            }
      case _ => throw new IllegalStateException

    override def specification: Specification.Array = ???

    override def print(as: Chain[A]): Option[Chain[Option[String]]] = of match
      case schema: Schema.Value[A] => as.map(schema.print).some
      case _                       => throw new IllegalStateException
