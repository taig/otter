package io.taig.otter

import cats.Id as Identity
import io.taig.otter as Base

object Plain extends Dsl[Identity]:
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)
  override def tuple[A, B](schema: Schema.Of[A, B]): Tuple.Of[schema.type, B] = Base.Tuple.One(schema)
