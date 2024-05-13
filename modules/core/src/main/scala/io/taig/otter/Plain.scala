package io.taig.otter

import cats.Id as Identity
import io.taig.otter as Base
import io.taig.otter.validation.Constraint
import cats.Contravariant

object Plain extends Dsl[Identity]:
  override def primitive[A](tpe: Type[A]): Primitive[A] = ??? // Fix(Base.Primitive.Required.Root(tpe))

  // given SchemaInvariant[Schema.Of, Schema.Of] = new SchemaInvariant[Schema.Of, Schema.Of]:
  //   extension [A, B](self: Schema.Of[A, B]) override def toTuple: Tuple.Of[Schema.Of[A, B], B] = Base.Tuple.One(self)
