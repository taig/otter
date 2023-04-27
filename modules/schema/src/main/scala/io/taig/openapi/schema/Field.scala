package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import cats.{Eq, Eval}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Violation}

abstract class Field[A](val metadata: Field.Metadata[A], val schema: Eval[Schema[?]]):
  self =>

  object default:
    def value: Option[A] = metadata.default
    def modify(f: Option[A] => Option[A]): Field[A] = self.copy(metadata.copy(default = f(value)))
    def as(value: Option[A]): Field[A] = modify(_ => value)

  object name:
    def value: String = metadata.name
    def modify(f: String => String): Field[A] = self.copy(metadata.copy(name = f(value)))
    def as(value: String): Field[A] = modify(_ => value)

  object nulls:
    def value: Field.Null = metadata.nulls
    def modify(f: Field.Null => Field.Null): Field[A] = self.copy(metadata.copy(nulls = f(value)))
    def as(value: Field.Null): Field[A] = modify(_ => value)
    def hide: Field[A] = as(Field.Null.Hide)
    def inherit: Field[A] = as(Field.Null.Inherit)
    def show: Field[A] = as(Field.Null.Show)

  final def copy(metadata: Field.Metadata[A]): Field[A] =
    new Field(metadata, schema) { export self.{decode, encode} }

  final def optional: Field[Option[A]] = new Field[Option[A]](metadata.map(_.some), self.schema):
    override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Option[A])] =
      openapi.get(metadata.name) match
        case Some(_) => self.decode(openapi).map(_.map(_.some))
        case None    => (openapi, none[A]).valid
    override def encode(a: Option[A], parent: Product.Null): OpenApi.Object = a match
      case Some(a) => self.encode(a, parent)
      case None =>
        val dropNull = (metadata.nulls, parent) match
          case (Field.Null.Inherit, Product.Null.Hide) | (Field.Null.Hide, _) => true
          case _                                                              => false

        if dropNull then OpenApi.Object.Empty else OpenApi.Object.one(metadata.name, OpenApi.Null)

  final transparent inline infix def zip[B](field: Field[B]): Product[?] = toProduct zip field.toProduct
  final transparent inline def :*[B](field: Field[B]): Product[?] = toProduct :* field

  final def toProduct: Product[A] = Product.fromField(this)

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)]

  def encode(a: A, parent: Product.Null): OpenApi.Object

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    val Default: Field.Null = Null.Inherit

    given Eq[Null] = Eq.fromUniversalEquals

  final case class Metadata[A](default: Option[A], name: String, nulls: Field.Null):
    def map[B](f: A => B): Field.Metadata[B] = copy(default = default.map(f))

  object Metadata:
    def empty[A](name: String): Field.Metadata[A] = Metadata(None, name, Null.Default)

  def apply[A](name: String, of: Eval[Schema[A]]): Field[A] = new Field[A](Metadata.empty(name), of):
    override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
      openapi.get(metadata.name) match
        case Some(value) =>
          of.value
            .decode(value)
            .bimap(_.modifyHistory(metadata.name /: _), (openapi.remove(metadata.name), _))
        case None =>
          val constraint = Constraint("required", reference = none)
          val violations = Violations.rootNec(Violation(constraint, actual = OpenApi.Null))
          metadata.default.toValid(violations).tupleLeft(openapi)

    override def encode(a: A, parent: Product.Null): OpenApi.Object =
      OpenApi.Object.one(metadata.name, of.value.encode(a))
