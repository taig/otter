package io.taig.otter

import io.taig.otter as Base

trait Plain extends Dsl:
  override object metadata extends Metadata:
    override type Schema = Any
    override type Collection = Any
    override type Enumeration = Any
    override type Primitive = Any

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Base.Primitive.Required.Root((), tpe)

object Plain extends Plain
