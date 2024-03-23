package io.taig.otter

sealed abstract class Product[+A, B] extends Schema[A, B]:
  final override type Self[+a, b] = Product[a, b]
  final override type Optional[+a, b] = Product[a, b]

  final override def imap[C](f: B => C)(g: C => B): Product[A, C] = ???
  final override def update[C](f: A => C): Product[C, B] = ???
  final override def optional: Product[A, Option[B]] = Product.Optional(this)

object Product:
  final case class Empty[A](metadata: A) extends Product[A, Unit]

  abstract class Root[A, B](metadata: A, schema: Schema[A, B]) extends Product[A, B]

  final case class Optional[A, B](product: Product[A, B]) extends Product[A, Option[B]]:
    export product.metadata
