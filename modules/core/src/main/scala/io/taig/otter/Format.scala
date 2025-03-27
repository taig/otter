package io.taig.otter

object Format:
  sealed abstract class Any extends Product with Serializable

  sealed abstract class Value extends Format.Any

  sealed abstract class Primitive extends Format.Value

  sealed abstract class Boolean extends Format.Primitive

  sealed abstract class Number extends Format.Primitive

  sealed abstract class String extends Format.Primitive

  sealed abstract class Array[+A <: Format.Any] extends Format.Value

  sealed abstract class Object[+A <: Format.Any] extends Format.Value

  sealed abstract class Null extends Format.Any
