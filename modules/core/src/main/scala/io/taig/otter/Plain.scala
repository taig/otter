package io.taig.otter

import cats.Id as Identity
import io.taig.otter as Base

object Plain extends Dsl[Identity]:
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)

  given SchemaInvariant[Schema.Of] = new SchemaInvariant[Schema.Of] {

    extension [A, B](self: Schema.Of[A, B]) override def toTuple: Tuple.Of[Schema.Of[A, B], B] = Base.Tuple.One(self)

  }
