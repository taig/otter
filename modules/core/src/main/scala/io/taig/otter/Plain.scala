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

  override def modifySchema[A, B, C, D](schema: Schema.Of[A, B])(
      f: Base.Schema[AsSchema, A, B] => Base.Schema[AsSchema, C, D]
  ): Schema.Of[C, D] = f(schema)

  override def modifyPrimitive[A, B](schema: Primitive[A])(
      f: Base.Primitive[A] => Base.Primitive[B]
  ): Base.Primitive[B] = f(schema)

  override def modifyPrimitiveRequired[A, B](schema: Primitive.Required[A])(
      f: Base.Primitive.Required[A] => Base.Primitive[B]
  ): Base.Primitive[B] = f(schema)

object Plain extends Plain
