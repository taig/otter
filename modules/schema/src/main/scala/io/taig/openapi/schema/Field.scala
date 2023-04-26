package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import cats.{Eq, Eval}
import io.taig.openapi.OpenApi

final case class Field[A](metadata: Field.Metadata[A], schema: Eval[Schema[A]]):
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

  def optional: Field[Option[A]] = copy(metadata = metadata.map(_.some), schema = schema.map(_.optional))

  infix def zip[B](field: Field[B]): Product[(A, B)] = toProduct zip field.toProduct
  transparent inline def :*[B](field: Field[B]): Product[?] = toProduct :* field

  def toProduct: Product[A] = Product.fromField(this)

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
    schema.value
      .decode(openapi.getOrNull(metadata.name))
      .bimap(_.modifyHistory(metadata.name /: _), (openapi.remove(metadata.name), _))

  def encode(a: A, parent: Product.Null): OpenApi.Object =
    val dropNull = (metadata.nulls, parent) match
      case (Field.Null.Inherit, Product.Null.Hide) | (Field.Null.Hide, _) => true
      case _                                                              => false

    schema.value.encode(a) match
      case OpenApi.Null if dropNull => OpenApi.Object.Empty
      case value                    => OpenApi.Object.one(metadata.name, value)

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

  def apply[A](name: String, schema: Eval[Schema[A]]): Field[A] = Field(Metadata.empty(name), schema)
