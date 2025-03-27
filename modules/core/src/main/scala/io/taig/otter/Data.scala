package io.taig.otter

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object Data:
  type Any = Data.Value | Data.Null

  type Value = Data.Primitive | Data.Object[?] | Data.Array[?]

  type Primitive = Number | Boolean | String

  type Number = JBigDecimal | JBigInteger | Long | Int | Float | Double

  final case class Object[+A <: Data.Any](values: List[(String, A)]) extends AnyVal

  final case class Array[+A <: Data.Any](values: Vector[A]) extends AnyVal

  type Null = Data.Null.type
  case object Null
