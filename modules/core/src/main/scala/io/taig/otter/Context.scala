package io.taig.otter

trait Context:
  type Codec
  def codec: Context.Codec[Codec, Product]

  type Primitive <: Codec
  def primitive: Context.Primitive[Primitive]

  type Product <: Codec
  def product: Context.Product[Product]

object Context:
  abstract class Field[A, B]:
    def value: B
    def set(value: B): A

  trait Codec[A, Product]:
    def toProduct(a: A): Product

  trait Primitive[A]:
    def empty: A

  trait Product[A]:
    def empty: A
    def zip(left: A, right: A): A
