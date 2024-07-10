package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity

private val applicativeComonad = ApplicativeComonad[Identity]

trait Plain extends Dsl:
  override object container extends Container:
    override type Schema[+A] = A
    override type Collection[+A] = A
    override type Dictionary[+A] = A
    override type Dynamic[+A] = A
    override type Enumeration[+A] = A
    override type Primitive[+A] = A
    override type Product[+A] = A
    override type Record[+A] = A
    override type Sum[+A] = A
    override type Union[+A] = A

  // implicit override def schemaApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  // implicit override def collectionApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  // implicit override def primitiveApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad
  // implicit override def unionApplicativeComonad: ApplicativeComonad[Identity] = applicativeComonad

object Plain extends Plain
