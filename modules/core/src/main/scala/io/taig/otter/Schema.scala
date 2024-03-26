package io.taig.otter

import io.taig.otter.Primitive.Required.Update

sealed abstract class Schema[+M, A]:
  type Of <: Schema[?, ?]
  type Self[+m, a] <: Schema.Of[Of, m, a]
  type Optional[+m, a] <: Schema.Of[Of, m, a]

  def metadata: M
  def update[N](f: M => N): Self[N, A]
  def imap[B](f: A => B)(g: B => A): Self[M, B]
  def optional: Optional[M, Option[A]]

  def toProductN[N](f: M => N): Product.Of[this.type, N, A] = Product.One(f(metadata), this)

object Schema:
  type Of[S <: Schema[?, ?], +M, A] = Schema[M, A] { type Of <: S }

sealed abstract class Primitive[+M, A] extends Schema[M, A]:
  self =>
  final override type Of = Primitive[?, ?]
  override type Self[+m, a] <: Primitive[m, a]
  final override type Optional[m, a] = Primitive[m, a]
  def tpe: Type[?]
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+M, A] extends Primitive[M, A]:
    final override type Self[+m, a] = Primitive.Required[m, a]
    final override def update[N](f: M => N): Primitive.Required[N, A] = Primitive.Required.Update(this, f)
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[M, B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]

    final case class Modify[M, A, B](primitive: Primitive.Required[M, A], f: A => B, g: B => A)
        extends Primitive.Required[M, B]:
      export primitive.{metadata, tpe}
    final case class Update[M, N, A](primitive: Primitive.Required[M, A], f: M => N) extends Primitive.Required[N, A]:
      export primitive.tpe
      override def metadata: N = f(primitive.metadata)

  sealed abstract class Optional[+M, A] extends Primitive[M, A]:
    final override type Self[+m, a] = Primitive[m, a]
    final override def update[N](f: M => N): Primitive.Optional[N, A] = Primitive.Optional.Update(this, f)
    final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[M, B] =
      Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Modify[M, A, B](primitive: Primitive[M, A], f: A => B, g: B => A) extends Primitive.Optional[M, B]:
      export primitive.{metadata, tpe}

    final case class Root[M, A](primitive: Primitive[M, A]) extends Primitive.Optional[M, Option[A]]:
      export primitive.{metadata, tpe}
    final case class Update[M, N, A](primitive: Primitive[M, A], f: M => N) extends Primitive.Optional[N, A]:
      export primitive.tpe
      override def metadata: N = f(primitive.metadata)

sealed abstract class Product[+M, A] extends Schema[M, A]:
  final override type Self[+m, a] = Product.Of[Of, m, a]
  final override type Optional[+m, a] = Product.Of[Of, m, a]
  final override def update[N](f: M => N): Product.Of[Of, N, A] = Product.Update(this, f)
  final override def imap[B](f: A => B)(g: B => A): Product.Of[Of, M, B] = Product.Modify(this, f, g)
  final override def optional: Product.Of[Of, M, Option[A]] = Product.Optional(this)

  final def zipWithN[M1 >: M, N, B](f: (M, M1) => N)(product: Product[M1, B]): Product.Of[Of | product.Of, N, (A, B)] =
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

  final case class Update[S <: Schema[?, ?], M, N, A](product: Product.Of[S, M, A], f: M => N) extends Product[N, A]:
    export product.Of
    override def metadata: N = f(product.metadata)

  final case class Zip[S <: Schema[?, ?], M, A, T <: Schema[?, ?], B](
      metadata: M,
      left: Product.Of[S, ?, A],
      right: Product.Of[T, ?, B]
  ) extends Product[M, (A, B)]:
    override type Of = left.Of | right.Of
