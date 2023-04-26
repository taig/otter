package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import cats.{Eq, Eval}
import io.taig.openapi.OpenApi

final case class Field[A](metadata: Field.Metadata, schema: Eval[Schema[A]]):
  self =>

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

  def optional: Field[Option[A]] = copy(schema = schema.map(_.optional))

  infix def zip[B](field: Field[B]): Product[(A, B)] = toProduct zip field.toProduct
  transparent inline def :*[B](field: Field[B]): Product[?] = toProduct :* field

  def toProduct: Product[A] = Product.fromField(this)

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
    schema.value
      .decode(openapi.getOrNull(metadata.name))
      .bimap(_.modifyHistory(metadata.name /: _), (openapi.remove(metadata.name), _))

  def encode(a: A, parent: Product.Nulls): OpenApi.Object =
    val dropNull = (metadata.nulls, parent) match
      case (Field.Null.Inherit, Product.Nulls.Hide) | (Field.Null.Hide, _) => true
      case _                                                               => false

    schema.value.encode(a) match
      case OpenApi.Null if dropNull => OpenApi.Object.Empty
      case value                    => OpenApi.Object.one(metadata.name, value)

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    given Eq[Null] = Eq.fromUniversalEquals

  final case class Metadata(name: String, nulls: Field.Null)

  def apply[A](name: String, schema: Eval[Schema[A]]): Field[A] = Field(Metadata(name, Null.Inherit), schema)
