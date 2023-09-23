package io.taig.otter

import cats.data.Chain

import java.lang.String as JString
import scala.Boolean as SBoolean

sealed abstract class Data extends Product with Serializable

object Data:
  final case class String(value: JString) extends Data
  final case class Boolean(value: SBoolean) extends Data
  final case class Number(value: Int | Long | Float | Double | BigDecimal | BigInt) extends Data
  final case class Object(values: Chain[(JString, Data)]) extends Data
  final case class Array(values: Chain[Data]) extends Data
  case object Null extends Data
