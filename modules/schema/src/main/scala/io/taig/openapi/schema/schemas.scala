package io.taig.openapi.schema

import cats.Eval
import io.taig.openapi.schema.Primitive.Of as PrimitiveOf
import io.taig.openapi.OpenApi
import io.taig.validation.validations

import java.util.UUID

object schemas:
  object Schema:
    type Primitive[A] = PrimitiveOf[A, OpenApi.Primitive]
    object Primitive:
      type Of[A, B <: OpenApi] = PrimitiveOf[A, B]

  val bigDecimal: Schema.Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Schema.Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Schema.Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Schema.Primitive[Double] = Primitive(Type.Double).format.as("double")
  val int: Schema.Primitive[Int] = Primitive(Type.Int).format.as("int32")
  val float: Schema.Primitive[Float] = Primitive(Type.Float).format.as("float")
  val long: Schema.Primitive[Long] = Primitive(Type.Long).format.as("int64")
  val string: Schema.Primitive[String] = Primitive(Type.String)
  val uuid: Schema.Primitive[UUID] = string.ivalidate(validations.parser.uuid)(_.toString).format.as("uuid")

  def field[A](name: String, schema: => Schema[A]): Field[A] = Field(name, Eval.later(schema))
