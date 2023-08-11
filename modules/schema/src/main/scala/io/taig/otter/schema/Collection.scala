package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Collection[A] extends Schema[A]:
  self =>
  override type Self[a] = Collection.Of[Of, a]
  final override type Properties[a] = Collection.Properties[a]
  final override type Codec = OpenApi.Array
  type Of[a] <: Schema[a]

  def schema: Schema[?]

  override def copy(properties: Collection.Properties[A]): Collection.Of[Of, A] = new Collection[A]
    with Copy(properties):
    export self.{decodeArray, encode, parse, print, schema, Of}

  override def optional: Collection.Of[Of, Option[A]] = new Collection[Option[A]] with Optional:
    export self.{schema, Of}
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)
    override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Option[A]] =
      self.parse(values).map(_.some)
    override def print(a: Option[A]): Option[Chain[Option[String]]] = a.flatMap(self.print)

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] = new Collection[B]
    with Validate[B](validation):
    export self.{schema, Of}
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

  protected def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A]
  protected def print(as: A): Option[Chain[Option[String]]]

object Collection:
  type Of[F[a] <: Schema[a], A] = Collection[A] { type Of[a] = F[a] }

  extension [A](self: Collection.Of[Schema.Value, A])
    def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A] = self.parse(values)
    def print(as: A): Option[Chain[Option[String]]] = self.print(as)

  final case class Properties[+A](description: Option[String], example: Option[A]) extends Schema.Properties[A]:
    override type Self[a] = Collection.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Collection.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Collection.Properties[B] = copy(example = f(example))
    override def flatMap[B](f: A => Option[B]): Collection.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Collection.Properties[Nothing] = Properties(None, None)

  def apply[F[a] <: Schema[a], A](of: => F[A]): Collection.Of[F, Chain[A]] = new Collection[Chain[A]]:
    override type Of[a] = F[a]
    override def schema: Schema[?] = of
    override def properties: Collection.Properties[Chain[A]] = Properties.Default
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
    override def print(as: Chain[A]): Option[Chain[Option[String]]] = of match
      case schema: Schema.Value[A] => as.map(schema.print).some
      case _                       => throw new IllegalStateException
