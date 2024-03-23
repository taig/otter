package io.taig.otter

sealed abstract class Product[+M, A] extends Codec[M, A]:
  self =>
  type Of <: Codec[?, ?]
  final override type Metadata = Product.Metadata
  final override type Self[+m, a] = Product.Of[self.Of, m, a]
  final override type Optional[+m, a] = Product.Of[self.Of, m, a]

  final override def imap[B](f: A => B)(g: B => A): Self[M, B] = Product.Modify(this, f, g)
  final override def update[N <: Product.Metadata](f: M => N): Self[N, A] = Product.Update(this, f)
  final override def optional: Self[M, Option[A]] = Product.Optional(this)

  final def zip[N >: M <: Product.Metadata, B](product: Product[N, B]): Product.Of[self.Of | product.Of, N, (A, B)] =
    Product.Zip(this, product)

object Product:
  type Of[C <: Codec[?, ?], +M, A] = Product[M, A] { type Of <: C }

  trait Metadata extends Codec.Metadata:
    override type Self <: Product.Metadata

    def zip(product: Self): Self

  object Metadata:
    type Aux[S <: Product.Metadata] = Product.Metadata { type Self = S }

  final case class Empty[M <: Product.Metadata](metadata: M) extends Product[M, Unit]:
    override type Of = Nothing

  final case class Modify[C <: Codec[?, ?], M, A, B](
      product: Product.Of[C, M, A],
      f: A => B,
      g: B => A
  ) extends Product[M, B]:
    export product.{metadata, Of}

  final case class One[M <: Product.Metadata, A](metadata: M, codec: Codec[M, A]) extends Product[M, A]:
    override type Of = codec.type

  final case class Optional[C <: Codec[?, ?], M, A](product: Product.Of[C, M, A]) extends Product[M, Option[A]]:
    export product.{metadata, Of}

  final case class Update[C <: Codec[?, ?], M, N <: Product.Metadata, A](
      product: Product.Of[C, M, A],
      f: M => N
  ) extends Product[N, A]:
    export product.Of
    override def metadata: N = f(product.metadata)

  final case class Zip[N <: Product.Metadata, C1 <: Codec[?, ?], C2 <: Codec[?, ?], A, B](
      left: Product.Of[C1, Metadata.Aux[N], A],
      right: Product.Of[C2, N, B]
  ) extends Product[N, (A, B)]:
    override type Of = left.Of | right.Of
    override def metadata: N = left.metadata.zip(right.metadata)
