package io.taig.crock.schema

import cats.{Eval, Hash}
import cats.data.Chain
import io.taig.crock.validation.validations
import io.taig.enumeration.ext.{EnumerationValues, Mapping}
import org.typelevel.ci.CIString

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Schema.Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Schema.Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Schema.Primitive(Type.Boolean)
  val double: Primitive[Double] = Schema.Primitive(Type.Double).format("double")
  val int: Primitive[Int] = Schema.Primitive(Type.Int).format("int32")
  val float: Primitive[Float] = Schema.Primitive(Type.Float).format("float")
  val long: Primitive[Long] = Schema.Primitive(Type.Long).format("int64")
  val string: Primitive[String] = Schema.Primitive(Type.String)
  val password: Primitive[String] = string.format("password")
  val uuid: Primitive[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  val date: Primitive[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  val dateTime: Primitive[LocalDateTime] = string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  val cistring: Primitive[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")

  def field[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Field[A, B] =
    Field(name, Eval.later(key), Eval.later(schema))
  def field[A](name: String, schema: => Schema[A]): Field[String, A] = field(name, string, schema)
  def field[A](name: Int, schema: => Schema[A]): Field[Int, A] = field(name, int, schema)

////  def branch[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Branch[A, B] =
////    Branch(name, Eval.later(key), Eval.later(schema))
////  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] = branch(name, string, schema)
////  def branch[A](name: Int, schema: => Schema[A]): Branch[Int, A] = branch(name, int, schema)

  object collection:
    def chain[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, Chain[A]] =
      Schema.Collection(Eval.later(schema))
    def vector[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, Vector[A]] =
      chain(schema).imap(_.toVector)(Chain.fromSeq)
    def list[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, List[A]] =
      chain(schema).imap(_.toList)(Chain.fromSeq)

  def enumeration[A, B](schema: => Value[A])(using mapping: Mapping[B, A]): Enumeration[B] =
    Schema.Enumeration(Eval.later(schema), mapping)
  def enumeration[A: Hash, B](schema: => Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration[B] = enumeration(schema)(using Mapping.enumeration(f))
