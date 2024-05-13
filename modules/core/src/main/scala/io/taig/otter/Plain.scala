package io.taig.otter

import cats.Id as Identity
import io.taig.otter as Base

object Plain extends Dsl[Identity]:
  override def primitive[A](tpe: Type[A]): Primitive[A] = ??? // Fix(Base.Primitive.Required.Root(tpe))
