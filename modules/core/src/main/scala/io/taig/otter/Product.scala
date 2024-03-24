package io.taig.otter

sealed abstract class Product[+A] extends Schema[A]:
  self =>
  type Of <: Schema[?]

  final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Product.Of[self.Of, B] = ??? // Product.Modify(this, f, g)
  final override def optional: Product.Of[self.Of, Option[A]] = ??? // Product.Optional(this)

  final def zip[B](product: Product[B]): Product.Of[self.Of | product.Of, (A, B)] =
    Product.Zip(this, product)

object Product:
  type Of[+C <: Schema[?], +A] = Product[A] { type Of <: C }

  case object Empty extends Product[Unit]:
    override type Of = Nothing

  final case class Modify[S <: Schema[?], A, B](product: Product.Of[S, A], f: A => B, g: B => A) extends Product[B]:
    export product.Of

  final case class One[S <: Schema[A], A](schema: S) extends Product[A]:
    override type Of = S

  // final case class Optional[C <: Codec[?, ?], M, A](product: Product.Of[C, M, A]) extends Product[M, Option[A]]:
  //   export product.{metadata, Of}

  // final case class Update[C <: Codec[?, ?], M, N, A](product: Product.Of[C, M, A], f: M => N) extends Product[N, A]:
  //   export product.Of
  //   override def metadata: N = f(product.metadata)

  final case class Zip[S1 <: Schema[?], S2 <: Schema[?], A, B](
      left: Product.Of[S1, A],
      right: Product.Of[S2, B]
  ) extends Product[(A, B)]:
    override type Of = left.Of | right.Of
