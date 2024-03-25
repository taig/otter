package io.taig.otter

sealed abstract class Schema[A]:
  type Of <: Schema[?]

  def imap[B](f: A => B)(g: B => A): Schema[B]
  def optional: Schema[Option[A]]

  def toProduct: Product.Of[this.type, A] = Product.One(this)

object Schema:
  type Of[S <: Schema[?], A] = Schema[A] { type Of <: S }

sealed abstract class Primitive[A] extends Schema[A]:
  override type Of <: Primitive[?]
  override def imap[B](f: A => B)(g: B => A): Primitive[B]
  final override def optional: Primitive.Optional[Option[A]] = Primitive.Optional.Root(this)

  def tpe: Type[?]

object Primitive:
  sealed abstract class Required[A] extends Primitive[A]:
    override type Of = Primitive.Required[?]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[A] extends Primitive[A]:
    override type Of = Primitive.Optional[?]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[B] =
      Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Modify[A, B](primitive: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]:
      export primitive.tpe

    final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export primitive.tpe

sealed abstract class Product[A] extends Schema[A]:
  final override def imap[B](f: A => B)(g: B => A): Product.Of[Of, B] = Product.Modify(this, f, g)
  final override def optional: Product.Of[Of, Option[A]] = Product.Optional(this)

  final def zip[B](product: Product[B]): Product.Of[Of | product.Of, (A, B)] = Product.Zip(this, product)

object Product:
  type Of[S <: Schema[?], A] = Product[A] { type Of <: S }

  case object Empty extends Product[Unit]:
    override type Of = Nothing

  final case class Modify[S <: Schema[?], A, B](product: Product.Of[S, A], f: A => B, g: B => A) extends Product[B]:
    export product.Of

  final case class One[S <: Schema[A], A](schema: S) extends Product[A]:
    override type Of = S

  final case class Optional[S <: Schema[?], A](product: Product.Of[S, A]) extends Product[Option[A]]:
    export product.Of

  final case class Zip[S <: Schema[?], A, T <: Schema[?], B](left: Product.Of[S, A], right: Product.Of[T, B])
      extends Product[(A, B)]:
    override type Of = left.Of | right.Of
