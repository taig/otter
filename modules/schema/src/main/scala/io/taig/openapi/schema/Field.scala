package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import cats.{Eq, Eval}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Violation}

abstract class Field[A, B](val key: Eval[Value[A]], val metadata: Field.Metadata[A, B], val schema: Eval[Schema[?]]):
  self =>

  object default:
    def value: Option[B] = metadata.default
    def modify(f: Option[B] => Option[B]): Field[A, B] = self.copy(metadata.copy(default = f(value)))
    def as(value: Option[B]): Field[A, B] = modify(_ => value)

  object name:
    def value: A = metadata.name
    def modify(f: A => A): Field[A, B] = self.copy(metadata.copy(name = f(value)))
    def as(value: A): Field[A, B] = modify(_ => value)

  object nulls:
    def value: Field.Null = metadata.nulls
    def modify(f: Field.Null => Field.Null): Field[A, B] = self.copy(metadata.copy(nulls = f(value)))
    def as(value: Field.Null): Field[A, B] = modify(_ => value)
    def hide: Field[A, B] = as(Field.Null.Hide)
    def inherit: Field[A, B] = as(Field.Null.Inherit)
    def show: Field[A, B] = as(Field.Null.Show)

  final def copy(metadata: Field.Metadata[A, B]): Field[A, B] =
    new Field[A, B](key, metadata, schema) { export self.{decode, encode} }

  final def optional: Field[A, Option[B]] = new Field[A, Option[B]](key, metadata.map(_.some), schema):
    override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Option[B])] =
      openapi.get(key.value.render(metadata.name)) match
        case Some(_) => self.decode(openapi).map(_.map(_.some))
        case None    => (openapi, none[B]).valid
    override def encode(a: Option[B], parent: Product.Null): OpenApi.Object = a match
      case Some(a) => self.encode(a, parent)
      case None =>
        val dropNull = (metadata.nulls, parent) match
          case (Field.Null.Inherit, Product.Null.Hide) | (Field.Null.Hide, _) => true
          case _                                                              => false

        if dropNull then OpenApi.Object.Empty else OpenApi.Object.one(key.value.render(metadata.name), OpenApi.Null)

  // TODO imap, ivalidate, ...

  final transparent inline infix def zip[C](field: Field[A, C]): Product[A, ?] = toProduct zip field.toProduct
  final transparent inline infix def :*[C](field: Field[A, C]): Product[A, ?] = toProduct :* field

  final def toProduct: Product[A, B] = Product(this)

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)]

  def encode(a: B, parent: Product.Null): OpenApi.Object

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    val Default: Field.Null = Null.Inherit

    given Eq[Null] = Eq.fromUniversalEquals

  final case class Metadata[A, B](default: Option[B], name: A, nulls: Field.Null):
    def map[C](f: B => C): Field.Metadata[A, C] = copy(default = default.map(f))

  object Metadata:
    def empty[A, B](name: A): Field.Metadata[A, B] = Metadata(None, name, Null.Default)

  def apply[A, B](name: A, ofKey: Eval[Value[A]], ofSchema: Eval[Schema[B]]): Field[A, B] =
    new Field[A, B](ofKey, Metadata.empty(name), ofSchema):
      override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
        val key = ofKey.value.render(metadata.name)

        openapi.get(key) match
          case Some(value) =>
            ofSchema.value
              .decode(value)
              .bimap(_.modifyHistory(key /: _), (openapi.remove(key), _))
          case None =>
            val constraint = Constraint("required", reference = none)
            val violations = Violations.rootNec(Violation(constraint, actual = OpenApi.Null))
            metadata.default.toValid(violations).tupleLeft(openapi)

      override def encode(b: B, parent: Product.Null): OpenApi.Object =
        OpenApi.Object.one(ofKey.value.render(metadata.name), ofSchema.value.encode(b))
