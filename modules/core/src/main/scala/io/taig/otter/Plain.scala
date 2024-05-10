package io.taig.otter

import io.taig.otter as Base

object Plain extends Dsl[Fix]:
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Fix(Base.Primitive.Required(tpe))
