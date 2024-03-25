package io.taig.otter

sealed abstract class Schema[M, A]:
  type Of <: Schema[?, ?]

  def metadata: M

  def imap[B](f: A => B)(g: B => A): Schema[M, B]
  def optional: Schema[M, Option[A]]

  def toProductWith[N](f: M => N): Product.Of[this.type, N, A] = Product.One(f(metadata), this)

object Schema:
  type Of[S <: Schema[?, ?], M, A] = Schema[M, A] { type Of <: S }

sealed abstract class Primitive[M, A] extends Schema[M, A]:
  override type Of <: Primitive[?, ?]
  override def imap[B](f: A => B)(g: B => A): Primitive[M, B]
  final override def optional: Primitive.Optional[M, Option[A]] = Primitive.Optional.Root(this)

  def tpe: Type[?]

object Primitive:
  sealed abstract class Required[M, A] extends Primitive[M, A]:
    override type Of = Primitive.Required[?, ?]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[M, B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]

    final case class Modify[M, A, B](primitive: Primitive.Required[M, A], f: A => B, g: B => A)
        extends Primitive.Required[M, B]:
      export primitive.{metadata, tpe}

  sealed abstract class Optional[M, A] extends Primitive[M, A]:
    override type Of = Primitive.Optional[?, ?]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[M, B] =
      Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Modify[M, A, B](primitive: Primitive[M, A], f: A => B, g: B => A) extends Primitive.Optional[M, B]:
      export primitive.{metadata, tpe}

    final case class Root[M, A](primitive: Primitive[M, A]) extends Primitive.Optional[M, Option[A]]:
      export primitive.{metadata, tpe}

sealed abstract class Product[M, A] extends Schema[M, A]:
  final override def imap[B](f: A => B)(g: B => A): Product.Of[Of, M, B] = Product.Modify(this, f, g)
  final override def optional: Product.Of[Of, M, Option[A]] = Product.Optional(this)

  final def zipWith[B](f: (M, M) => M)(product: Product[M, B]): Product.Of[Of | product.Of, M, (A, B)] =
    Product.Zip(f(metadata, product.metadata), this, product)

object Product:
  type Of[S <: Schema[?, ?], M, A] = Product[M, A] { type Of <: S }

  final case class Empty[M](metadata: M) extends Product[M, Unit]:
    override type Of = Nothing

  final case class Modify[S <: Schema[?, ?], M, A, B](product: Product.Of[S, M, A], f: A => B, g: B => A)
      extends Product[M, B]:
    export product.{metadata, Of}

  final case class One[S <: Schema[?, A], M, A](metadata: M, schema: S) extends Product[M, A]:
    override type Of = S

  final case class Optional[S <: Schema[?, ?], M, A](product: Product.Of[S, M, A]) extends Product[M, Option[A]]:
    export product.{metadata, Of}

  final case class Zip[S <: Schema[?, ?], M, A, T <: Schema[?, ?], B](
      metadata: M,
      left: Product.Of[S, M, A],
      right: Product.Of[T, M, B]
  ) extends Product[M, (A, B)]:
    override type Of = left.Of | right.Of
