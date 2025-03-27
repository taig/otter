package io.taig.otter

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object Data:
  type Number = JBigDecimal | JBigInteger | Long | Int | Float | Double
  type Primitive = Number | Boolean | String
  final case class Object[+A <: Data.Any](values: List[(String, A)]) extends AnyVal
  final case class Array[+A <: Data.Any](values: List[A]) extends AnyVal
  
  type Value = Data.Primitive | Data.Object[?] | Data.Array[?]
  
  type Null = Data.Null.type
  case object Null

  type Any = Data.Value | Data.Null
