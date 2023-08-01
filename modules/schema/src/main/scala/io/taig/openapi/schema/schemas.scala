package io.taig.openapi.schema

import io.taig.openapi.validation.validations
import org.typelevel.ci.CIString

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Primitive[Double] = Primitive(Type.Double).format("double")
  val int: Primitive[Int] = Primitive(Type.Int).format("int32")
  val float: Primitive[Float] = Primitive(Type.Float).format("float")
  val long: Primitive[Long] = Primitive(Type.Long).format("int64")
  val string: Primitive[String] = Primitive(Type.String)
  val password: Primitive[String] = string.format("password")
  val uuid: Primitive[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  val date: Primitive[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  val dateTime: Primitive[LocalDateTime] = string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  val cistring: Primitive[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")

////  def field[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Field[A, B] =
////    Field(name, Eval.later(key), Eval.later(schema))
////  def field[A](name: String, schema: => Schema[A]): Field[String, A] = field(name, string, schema)
////  def field[A](name: Int, schema: => Schema[A]): Field[Int, A] = field(name, int, schema)
////
////  def branch[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Branch[A, B] =
////    Branch(name, Eval.later(key), Eval.later(schema))
////  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] = branch(name, string, schema)
////  def branch[A](name: Int, schema: => Schema[A]): Branch[Int, A] = branch(name, int, schema)
////
////  object collection:
////    def vector[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Object.Of[Vector[A], B] =
////      Collection.Object(Eval.later(schema))
////    def vector[A, B <: OpenApi](schema: => Schema.Value[A]): Collection.Value[Vector[A]] =
////      Collection.Value(Eval.later(schema))
////
////  def enumeration[A, B](schema: => Schema.Value[A])(using mapping: Mapping[B, A]): Enumeration[B] =
////    Enumeration(Eval.later(schema), mapping)
////  def enumeration[A: Hash, B](schema: => Schema.Value[A])(f: B => A)(using
////      EnumerationValues.Aux[B, B]
////  ): Enumeration[B] =
////    enumeration(schema)(using Mapping.enumeration(f))
