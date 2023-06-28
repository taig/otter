package io.taig.openapi.schema

import cats.Eval
import io.taig.openapi.validation.validations

import java.util.UUID

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Primitive[Double] = Primitive(Type.Double).withFormat("double")
  val int: Primitive[Int] = Primitive(Type.Int).withFormat("int32")
  val float: Primitive[Float] = Primitive(Type.Float).withFormat("float")
  val long: Primitive[Long] = Primitive(Type.Long).withFormat("int64")
  val string: Primitive[String] = Primitive(Type.String)
  val uuid: Primitive[UUID] = string.ivalidate(validations.parser.uuid)(_.toString).withFormat("uuid")

  def field[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Field[A, B] =
    Field(name, Eval.later(key), Eval.later(schema))
  def field[A](name: String, schema: => Schema[A]): Field[String, A] = field(name, string, schema)
  def field[A](name: Int, schema: => Schema[A]): Field[Int, A] = field(name, int, schema)

  def branch[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Branch[A, B] =
    Branch(name, Eval.later(key), Eval.later(schema))
  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] = branch(name, string, schema)
  def branch[A](name: Int, schema: => Schema[A]): Branch[Int, A] = branch(name, int, schema)
