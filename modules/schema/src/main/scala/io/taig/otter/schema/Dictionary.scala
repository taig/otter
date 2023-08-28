package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.{OpenApi, Specification}
import io.taig.otter.validation.{Constraint, Validation, Violation}

import scala.collection.immutable.VectorMap

sealed abstract class Dictionary[A] extends Schema[A]:
  self =>
  override type Self[a] = Dictionary[a]

  override final def modifySpecification(f: Specification.Value => Specification.Value): Dictionary[A] = ???

  final override def optional: Dictionary[Option[A]] = new Dictionary[Option[A]] with Optional:
    override def specification: Specification.Value = ???
    override def decodeNone: Validated[Violations, Option[A]] = none.valid
    override def decode(values: VectorMap[String, OpenApi]): Validated[Violations, Option[A]] =
      self.decode(values).map(_.some)
    override def encode(a: Option[A]): Option[OpenApi.Object] = a.flatMap(self.encode)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] = new Dictionary[B]
    with Validate[B](validation):
    override def specification: Specification.Value = ???
    override def decodeNone: Validated[Violations, B] = self.decodeNone.andThen(validation(_).leftMap(Violations.root))
    override def decode(values: VectorMap[String, OpenApi]): Validated[Violations, B] =
      self.decode(values).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Object] = self.encode(g(b))

  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi match
    case Some(OpenApi.Object(values)) => decode(values)
    case Some(openapi)                => Violations.rootNec(Violation.tpe("object", openapi.tpe)).invalid
    case None                         => decodeNone
  protected def decodeNone: Validated[Violations, A]
  protected def decode(values: VectorMap[String, OpenApi]): Validated[Violations, A]
  override def encode(a: A): Option[OpenApi.Object]

object Dictionary:
  def apply[A, B](a: => Schema.Value[A], b: => Schema[B]): Dictionary[VectorMap[A, B]] = new Dictionary:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeNone: Validated[Violations, VectorMap[A, B]] = Violations.rootNec(Violation.required).invalid
    def parse(value: String): Validated[Violations, A] =
      if value.isEmpty then a.parse(value.some).orElse(a.parse(none)) else a.parse(value.some)
    override def decode(values: VectorMap[String, OpenApi]): Validated[Violations, VectorMap[A, B]] =
      values.toSeq.traverse { case (key, value) => (parse(key), b.decode(value)).tupled }.map(_.to(VectorMap))
    override def encode(values: VectorMap[A, B]): Option[OpenApi.Object] = OpenApi
      .Object(values.map { case (key, value) => (a.print(key).orEmpty, b.encode(value).getOrElse(OpenApi.Null)) })
      .some
