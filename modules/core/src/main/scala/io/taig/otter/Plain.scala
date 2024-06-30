package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity

private val applicativeComonad = ApplicativeComonad[Identity]

trait Plain extends Dsl:
  override object container extends Container:
    override type Schema[+A] = A
    override type Collection[+A] = A
    override type Primitive[+A] = A
    override type Tuple[+A] = A
    override type Union[+A] = A

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)

  override given schemaApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  override given collectionApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  override given primitiveApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  override given unionApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad

object Plain extends Plain
