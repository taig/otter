package io.taig.otter

import io.taig.otter as Base

trait Plain extends Dsl:
  override object container extends Container:
    override type Schema[+A] = A
    override type Collection[+A] = A
    override type Primitive[+A] = A
    override type Tuple[+A] = A
    override type Union[+A] = A

  override given schemaApplicativeComonad: ApplicativeComonad[container.Schema] = ???

  override given collectionApplicativeComonad: ApplicativeComonad[container.Collection] = ???

  override given primitiveApplicativeComonad: ApplicativeComonad[container.Primitive] = ???

  override given unionApplicativeComonad: ApplicativeComonad[container.Union] = ???

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)

object Plain extends Plain
