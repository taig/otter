package io.taig.otter

sealed abstract class Schema[+A]:
  def imap[A1 >: A, B](f: A => B)(g: B => A1): Schema[B]
  def optional: Schema[Option[A]]

  def toProduct: Product.Of[this.type, A] = Product.One(this)

sealed abstract class Primitive[+A] extends Schema[A]:
  self =>
  override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive[B]
  final override def optional: Primitive.Optional[Option[A]] = Primitive.Optional.Root(this)

  def tpe: Type[?]

object Primitive:
  sealed abstract class Required[+A] extends Primitive[A]:
    self =>
    final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive.Required[B] =
      Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, A1 >: A, B](primitive: Primitive.Required[A], f: A => B, g: B => A1)
        extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[+A] extends Primitive[A]:
    self =>

    final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive.Optional[B] =
      Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Modify[A, A1 >: A, B](primitive: Primitive[A], f: A => B, g: B => A1)
        extends Primitive.Optional[B]:
      export primitive.tpe

    final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export primitive.tpe

sealed abstract class Product[+A] extends Schema[A]:
  self =>
  type Of <: Schema[?]

  final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Product.Of[self.Of, B] = Product.Modify(this, f, g)
  final override def optional: Product.Of[self.Of, Option[A]] = Product.Optional(this)

  final def zip[B](product: Product[B]): Product.Of[self.Of | product.Of, (A, B)] =
    Product.Zip(this, product)

object Product:
  type Of[+S <: Schema[?], +A] = Product[A] { type Of <: S }

  case object Empty extends Product[Unit]:
    override type Of = Nothing

  final case class Modify[S <: Schema[?], A, A1 >: A, B](product: Product.Of[S, A], f: A => B, g: B => A1)
      extends Product[B]:
    export product.Of

  final case class One[S <: Schema[A], A](schema: S) extends Product[A]:
    override type Of = S

  final case class Optional[C <: Schema[?], M, A](product: Product.Of[C, A]) extends Product[Option[A]]:
    export product.Of

  final case class Zip[S1 <: Schema[?], S2 <: Schema[?], A, B](left: Product.Of[S1, A], right: Product.Of[S2, B])
      extends Product[(A, B)]:
    override type Of = left.Of | right.Of
