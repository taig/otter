package io.taig.otter

import io.taig.otter as Base

trait Plain extends Dsl:
  final override type AsSchema[+A] = A
  final override type AsCollection[+A] = A
  final override type AsPrimitive[+A] = A
  final override type AsTuple[+A] = A
  final override type AsUnion[+A] = A

  override protected inline def asPrimitive[A](a: A): AsPrimitive[A] = a
  override protected inline def asCollection[A](a: A): AsCollection[A] = a
  override protected inline def asTuple[A](a: A): AsTuple[A] = a

object Plain extends Plain
