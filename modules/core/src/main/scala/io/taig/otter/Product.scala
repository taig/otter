package io.taig.otter

sealed abstract class Product[+M, A] extends Codec[M, A]:
  self =>
  type Of <: Codec[?, ?]
  final override type Self[+m, a] = Product.Of[self.Of, m, a]
  final override type Optional[+m, a] = Product.Of[self.Of, m, a]

  final override def imap[B](f: A => B)(g: B => A): Product.Of[self.Of, M, B] = ??? // Product.Modify(this, f, g)
  final override def update[N](f: M => N): Product.Of[self.Of, N, A] = ??? // Product.Update(this, f)
  final override def optional: Product.Of[self.Of, M, Option[A]] = ??? // Product.Optional(this)

  final def zipWith[N >: M, B](merge: (M, N) => N)(
      product: Product[N, B]
  ): Product.Of[self.Of | product.Of, N, (A, B)] = ???
  // Product.Zip(merge(self.metadata, product.metadata), this, product)

object Product:
  type Of[+C <: Codec[?, ?], +M, A] = Product[M, A] { type Of <: C }

  final case class Empty[M](metadata: M) extends Product[M, Unit]:
    override type Of = Nothing

  // final case class Modify[C <: Codec[?, ?], M, A, B](product: Product.Of[C, M, A], f: A => B, g: B => A)
  //     extends Product[M, B]:
  //   export product.{metadata, Of}

  final case class One[C <: Codec[?, A], M, A](metadata: M, codec: C) extends Product[M, A]:
    override type Of = C

  // final case class Optional[C <: Codec[?, ?], M, A](product: Product.Of[C, M, A]) extends Product[M, Option[A]]:
  //   export product.{metadata, Of}

  // final case class Update[C <: Codec[?, ?], M, N, A](product: Product.Of[C, M, A], f: M => N) extends Product[N, A]:
  //   export product.Of
  //   override def metadata: N = f(product.metadata)

  // final case class Zip[M, C1 <: Codec[?, ?], C2 <: Codec[?, ?], A, B](
  //     metadata: M,
  //     left: Product.Of[C1, M, A],
  //     right: Product.Of[C2, M, B]
  // ) extends Product[M, (A, B)]:
  //   override type Of = left.Of | right.Of
