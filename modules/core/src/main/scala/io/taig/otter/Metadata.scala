package io.taig.otter

trait Metadata:
  type Codec

  type Primitive <: Codec
  def primitive: Metadata.Primitive[Primitive]

  type Product <: Codec
  def product: Metadata.Product[Product]

object Metadata:
  trait Primitive[A]:
    def empty: A

  trait Product[A]:
    def empty: A
    def zip(left: A, right: A): A

  trait Coproduct[A]:
    def empty: A
    def orElse(left: A, right: A): A
