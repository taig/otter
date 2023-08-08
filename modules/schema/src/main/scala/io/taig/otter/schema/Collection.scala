package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Collection[A] extends Schema[A]:
  self =>
  final override type Self[a] = Collection[a]
  final override type Properties[a] = Collection.Properties[a]
  final override type Codec = OpenApi.Array

  def schema: Schema[?]

  override def copy(properties: Collection.Properties[A]): Collection[A] = new Collection[A] with Copy(properties):
    export self.{decodeArray, schema}

  override def optional: Collection[Option[A]] = new Collection[Option[A]] with Optional:
    export self.schema
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[B] = new Collection[B]
    with Validate[B](validation):
    export self.schema
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, B] =
      self.decodeArray(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Array] = self.encode(g(b))
  final override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi
    .traverse(openapi => openapi.asArray.toValid(Violations.rootNec(Violation.tpe("array", openapi.tpe))))
    .andThen(decodeArray)
  protected def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, A]

object Collection:
  final case class Properties[+A](description: Option[String], example: Option[A]) extends Schema.Properties[A]:
    override type Self[a] = Collection.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Collection.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Collection.Properties[B] = copy(example = f(example))
    override def flatMap[B](f: A => Option[B]): Collection.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Collection.Properties[Nothing] = Properties(None, None)

  sealed abstract class Value[A] extends Collection[A]:
    self =>
    override def schema: Schema.Value[?]

    final override def copy(properties: Collection.Properties[A]): Collection.Value[A] = new Value[A]
      with Copy(properties):
      export self.{decodeArray, parse, print, schema}

    final override def optional: Collection.Value[Option[A]] = new Value[Option[A]] with Optional:
      export self.schema
      override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Option[A]] =
        openapi.traverse(openapi => self.decodeArray(openapi.some))
      override def encode(a: Option[A]): Option[OpenApi.Array] = a.flatMap(self.encode)
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Option[A]] =
        values.traverse(values => self.parse(values.some))
      override def print(a: Option[A]): Option[Chain[Option[String]]] = a.flatMap(self.print)

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Value[B] = new Value[B]
      with Validate(validation):
      export self.schema
      override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, B] =
        self.decodeArray(openapi).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): Option[OpenApi.Array] = self.encode(g(b))
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, B] =
        self.parse(values).andThen(validation(_).leftMap(Violations.root))
      override def print(b: B): Option[Chain[Option[String]]] = self.print(g(b))

    def parse(values: Option[Chain[Option[String]]]): Validated[Violations, A]
    def print(a: A): Option[Chain[Option[String]]]

  object Value:
    def apply[A](of: => Schema.Value[A]): Collection.Value[Chain[A]] = new Value[Chain[A]]:
      override def schema: Schema.Value[?] = of
      override def properties: Collection.Properties[Chain[A]] = Properties.Default
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Chain[A]] =
        Collection.decode(of, openapi)
      override def encode(as: Chain[A]): Option[OpenApi.Array] = Collection.encode(of, as)
      override def parse(values: Option[Chain[Option[String]]]): Validated[Violations, Chain[A]] =
        values
          .toValid(Violations.rootNec(Violation.required))
          .andThen: values =>
            values.zipWithIndex.traverse { case (value, index) => of.parse(value).leftMap(_.modifyHistory(index /: _)) }
      override def print(as: Chain[A]): Option[Chain[Option[String]]] = as.map(of.print).some

  def apply[A](of: => Schema[A]): Collection[Chain[A]] = new Collection[Chain[A]]:
    override def schema: Schema[?] = of
    override def properties: Collection.Properties[Chain[A]] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(openapi: Option[OpenApi.Array]): Validated[Violations, Chain[A]] =
      Collection.decode(of, openapi)
    override def encode(as: Chain[A]): Option[OpenApi.Array] = Collection.encode(of, as)

  private def decode[A](schema: Schema[A], openapi: Option[OpenApi.Array]): Validated[Violations, Chain[A]] = openapi
    .toValid(Violations.rootNec(Violation.required))
    .andThen: openapi =>
      openapi.toChain.zipWithIndex.traverse { case (value, index) =>
        schema.decode(value).leftMap(_.modifyHistory(index /: _))
      }

  private def encode[A](schema: Schema[A], as: Chain[A]): Option[OpenApi.Array] =
    OpenApi.Array(as.map(schema.encode(_).getOrElse(OpenApi.Null))).some
