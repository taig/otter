package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.{Eq, Eval}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.{Constraint, Validation}

import scala.deriving.*

final case class Field[A](name: String, nulls: Field.Null, schema: Eval[Schema[A]]):
  def constraints: Chain[Constraint[OpenApi]] = schema.value.constraints

  def modifyName(f: String => String): Field[A] = copy(name = f(name))
  def withName(name: String): Field[A] = modifyName(_ => name)

  def modifyNull(f: Field.Null => Field.Null): Field[A] = copy(nulls = f(nulls))
  def setNull(nul: Field.Null): Field[A] = modifyNull(_ => nul)
  def hideNull: Field[A] = setNull(Field.Null.Hide)
  def inheritNull: Field[A] = setNull(Field.Null.Inherit)
  def showNull: Field[A] = setNull(Field.Null.Show)

  def modifySchema[B](f: Schema[A] => Schema[B]): Field[B] = copy(schema = schema.map(f))

  infix def zip[B](field: Field[B]): Product[(A, B)] = toProduct zip field.toProduct
  def :*[B](field: Field[B]): Product[(A, B)] = zip(field)
  def *:[B](field: Field[B]): Product[(B, A)] = field.toProduct zip toProduct
  def <*(field: Field[Unit]): Product[A] = toProduct <* field.toProduct

  def imap[B](f: A => B)(g: B => A): Field[B] = modifySchema(_.imap(f)(g))
  def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] =
    imap(evidence.from)(evidence.to).toProduct
  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Field[C] =
    modifySchema(_.ivalidate(validation)(g))

  def toProduct: Product[A] = Product.one(this)

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
    schema.value
      .decode(openapi.getOrNull(name))
      .bimap(_.modifyHistory(name /: _), (openapi.remove(name), _))

  def encode(a: A, parent: Product.Nulls): OpenApi.Object =
    val dropNull = (nulls, parent) match
      case (Field.Null.Inherit, Product.Nulls.Hide) | (Field.Null.Hide, _) => true
      case _                                                               => false

    (schema.value.encode(a): OpenApi) match
      case OpenApi.Null if dropNull => OpenApi.Object.Empty
      case value                    => OpenApi.Object.one(name, value)

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    given Eq[Null] = Eq.fromUniversalEquals

  extension (self: Field[Unit]) def *>[B](field: Field[B]): Product[B] = self.toProduct *> field.toProduct

  def apply[A](name: String, schema: Eval[Schema[A]]): Field[A] = Field(name, Null.Inherit, schema)

  given InvariantValidation[Field] with
    override def imap[A, B](fa: Field[A])(f: A => B)(g: B => A): Field[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Field[B])(validation: Validation[A, B, B, C])(g: C => B): Field[C] =
      fa.ivalidate(validation)(g)
