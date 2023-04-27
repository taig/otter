package io.taig.openapi.schema

import cats.Eval
import io.taig.validation.validations

import java.util.UUID

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Primitive[Double] = Primitive(Type.Double).format.as("double")
  val int: Primitive[Int] = Primitive(Type.Int).format.as("int32")
  val float: Primitive[Float] = Primitive(Type.Float).format.as("float")
  val long: Primitive[Long] = Primitive(Type.Long).format.as("int64")
  val string: Primitive[String] = Primitive(Type.String)
  val uuid: Primitive[UUID] = string.ivalidate(validations.parser.uuid)(_.toString).format.as("uuid")

  def field[A](name: String, schema: => Schema[A]): Field[A] = Field(name, Eval.later(schema))
