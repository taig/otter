package io.taig.otter

sealed abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a]
  type Optional[a] <: Schema[a]
  type Of <: Schema[?]

  def imap[B](f: A => B)(g: B => A): Self[B]
  def optional: Optional[Option[A]]

  final def toProduct: Product.Of[this.type, A] = Product.One(this)

object Schema:
  type Of[S <: Schema[?], A] = Schema[A] { type Of = S }

sealed abstract class Primitive[A] extends Schema[A]:
  self =>
  final override type Of = Primitive[?]
  override type Self[a] <: Primitive[a]
  final override type Optional[a] = Primitive[a]
  def tpe: Type[?]
  final override def optional: Primitive[Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[A] extends Primitive[A]:
    final override type Self[a] = Primitive.Required[a]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[A] extends Primitive[A]:
    final override type Self[a] = Primitive[a]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[B] = Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Modify[A, B](primitive: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]:
      export primitive.tpe

    final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export primitive.tpe

abstract class Product[A] extends Schema[A]:
  final override type Self[a] = Product[a]
  final override type Optional[a] = Product[a]
  final override def imap[B](f: A => B)(g: B => A): Product[B] = Product.Modify(this, f, g)
  final override def optional: Product[Option[A]] = Product.Optional(this)

  final def zip[B](product: Product[B]): Product[(A, B)] = Product.Zip(this, product)

object Product:
  type Of[S <: Schema[?], A] = Product[A] { type Of = S }

  case object Empty extends Product[Unit]:
    override type Of = Nothing

  final case class Modify[A, B](product: Product[A], f: A => B, g: B => A) extends Product[B]:
    export product.Of

  final case class One[S <: Schema[A], A](schema: S) extends Product[A]:
    override type Of = S

  final case class Optional[A](product: Product[A]) extends Product[Option[A]]:
    export product.Of

  final case class Zip[A, B](left: Product[A], right: Product[B]) extends Product[(A, B)]:
    override type Of = left.Of | right.Of
