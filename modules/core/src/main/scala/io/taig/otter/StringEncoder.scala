package io.taig.otter

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.Id

object StringEncoder:
  def apply[A](schema: Base.Value.Required.Writer[container.Schema, ?, A], a: A) = schema match
    case schema: Base.Enumeration.Required.Writer[container.Schema, ?, A] => ???
    case schema: Primitive.Required.Writer[A]               => PrimitiveStringEncoder(schema)
    // case schema: Enumeration.Required[A] => EnumerationStringEncoder(schema)

object StringEncoder2:
  def apply[A](schema: Base.Value.Required.Writer[?, ?, ?]) = schema match
    case schema: Base.Enumeration.Required.Writer[?, ?, ?] => ???
    case schema: Primitive.Required.Writer[?]              => ???
    // case schema: Enumeration.Required[A] => EnumerationStringEncoder(schema)
