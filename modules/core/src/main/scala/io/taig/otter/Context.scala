package io.taig.otter

trait Context:
  type Codec
  def codec: Context.Codec[Codec, Product]

  type Primitive
  def primitive: Context.Primitive[Primitive]

  type Product
  def product: Context.Product[Product]

object Context:
  trait Codec[A, Product]:
    def toProduct(a: A): Product

  trait Primitive[A]:
    def empty: A

  trait Product[A]:
    def empty: A
    def zip(left: A, right: A): A
