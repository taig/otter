package io.taig.otter

import io.taig.otter as Base

trait Plain extends Dsl:
  override object metadata extends Metadata:
    override type Schema = Any
    override type Collection = Any
    override val collection = ()
    override type Enumeration = Any
    override val enumeration = ()
    override type Primitive = Any
    override val primitive = ()
    override type Tuple = Any
    override val tuple = ()
    override type Union = Any
    override val union = ()

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Base.Primitive.Required.Root((), tpe)

object Plain extends Plain
