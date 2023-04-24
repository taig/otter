package io.taig.openapi.schema

import cats.data.Validated
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

  infix def zip[B](field: Field[B]) = ???
  def :*[B](field: Field[B]) = ???

  def toProduct = ???

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] = ???

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    given Eq[Null] = Eq.fromUniversalEquals

  final case class Metadata(name: String, nulls: Field.Null)

  def apply[A](name: String, schema: Eval[Schema[A]]): Field[A] = Field(Metadata(name, Null.Inherit), schema)
