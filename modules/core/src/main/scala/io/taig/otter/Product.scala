package io.taig.otter

sealed abstract class Product[M, A] extends Schema[M, A]:
  self =>
  type Of <: Schema[?, ?]
  final override type Self[a] = Product.Of[self.Of, M, a]
  final override type Optional[a] = Product.Of[self.Of, M, a]

  final override def imap[B](f: A => B)(g: B => A): Product.Of[self.Of, M, B] = ??? // Product.Modify(this, f, g)
  final override def optional: Product.Of[self.Of, M, Option[A]] = ??? // Product.Optional(this)

  final def zipWith[B](f: (M, M) => M)(product: Product[M, B]): Product.Of[self.Of | product.Of, M, (A, B)] =
    Product.Zip(f(self.metadata, product.metadata), this, product)

object Product:
  type Of[+C <: Schema[?, ?], M, A] = Product[M, A] { type Of <: C }

  final case class Empty[M](metadata: M) extends Product[M, Unit]:
    override type Of = Nothing

  final case class Modify[S <: Schema[?, ?], M, A, B](product: Product.Of[S, M, A], f: A => B, g: B => A)
      extends Product[M, B]:
    export product.{metadata, Of}

  final case class One[S <: Schema[?, A], M, A](metadata: M, schema: S) extends Product[M, A]:
    override type Of = S

  // final case class Optional[C <: Codec[?, ?], M, A](product: Product.Of[C, M, A]) extends Product[M, Option[A]]:
  //   export product.{metadata, Of}

  // final case class Update[C <: Codec[?, ?], M, N, A](product: Product.Of[C, M, A], f: M => N) extends Product[N, A]:
  //   export product.Of
  //   override def metadata: N = f(product.metadata)

  final case class Zip[S1 <: Schema[?, ?], S2 <: Schema[?, ?], M, A, B](
      metadata: M,
      left: Product.Of[S1, M, A],
      right: Product.Of[S2, M, B]
  ) extends Product[M, (A, B)]:
    override type Of = left.Of | right.Of
