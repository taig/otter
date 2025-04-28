package io.taig.otter

import cats.Show
import cats.syntax.all.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object Data:
  type Any = Data.Value | Data.Null

  type Value = Data.Primitive | Data.Object[?] | Data.Array[?]

  object Value:
    given show: Show[Data.Value] =
      case value: Data.Primitive => Primitive.show.show(value)
      case value: Data.Object[?] => Object.show.show(value)
      case value: Data.Array[?]  => Array.show.show(value)

  type Primitive = Number | Boolean | String

  object Primitive:
    given show: Show[Data.Primitive] =
      case value: Data.Number => Number.show.show(value)
      case value: Boolean     => String.valueOf(value)
      case value: String      => s"\"$value\""

  type Number = JBigDecimal | JBigInteger | Long | Int | Float | Double

  object Number:
    given show: Show[Data.Number] =
      case value: JBigDecimal                                 => String.valueOf(value)
      case value: (JBigInteger | Float | Double | Long | Int) => String.valueOf(value)

  final case class Object[+A <: Data.Any](values: List[(String, A)]) extends AnyVal

  object Object:
    given show[A <: Data.Any]: Show[Data.Object[A]] = obj =>
      "{" + obj.values.map { case (key, value) => show"\"$key\":${Data.show.show(value)}" }.mkString(",") + "}"

  final case class Array[+A <: Data.Any](values: Vector[A]) extends AnyVal

  object Array:
    given show[A <: Data.Any]: Show[Data.Array[A]] = array => "[" + array.values.map(Data.show.show).mkString(",") + "]"

  type Null = Data.Null.type
  case object Null

  given show: Show[Data.Any] =
    case value: Data.Value => Value.show.show(value)
    case value: Data.Null  => "null"
