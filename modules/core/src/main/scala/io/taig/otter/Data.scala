package io.taig.otter

import cats.data.Chain

import java.lang.String as JString
import scala.Boolean as SBoolean

sealed abstract class Data extends Product with Serializable:
  final def asValue: Option[Data.Value] = this match
    case value: Data.Value => Some(value)
    case Data.Null         => None

  final def name: String = this match
    case _: Data.String  => "string"
    case _: Data.Boolean => "boolean"
    case _: Data.Number  => "number"
    case _: Data.Object  => "object"
    case _: Data.Array   => "array"
    case Data.Null       => "null"

object Data:
  sealed abstract class Value extends Data

  final case class String(value: JString) extends Data.Value
  final case class Boolean(value: SBoolean) extends Data.Value
  final case class Number(value: Int | Long | Float | Double | BigDecimal | BigInt) extends Data.Value
  final case class Object(values: Chain[(JString, Data)]) extends Data.Value
  final case class Array(values: Chain[Data]) extends Data.Value

  case object Null extends Data
