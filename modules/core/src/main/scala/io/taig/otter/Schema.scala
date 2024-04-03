package io.taig.otter

sealed abstract class Schema[+A, B]:
  self =>
  type Self[+a, b] <: Schema[a, b]
  type Optional[+a, b] <: Schema[a, b]
  type Of <: Schema[?, ?]

  def metadata: A
  def update[C](f: A => C): Self[C, B]
  def imap[C](f: B => C)(g: C => B): Self[A, C]
  def optional: Optional[A, Option[B]]

  final def toProduct[C](metadata: C): Product.Of[this.type, C, B] = Product.One(metadata, this)

object Schema:
  type Of[S <: Schema[?, ?], A, B] = Schema[A, B] { type Of <: S }

sealed abstract class Primitive[+A, B] extends Schema[A, B]:
  self =>
  override type Self[+a, b] <: Primitive[a, b]
  final override type Optional[+a, b] = Primitive[a, b]
  final override type Of = Primitive[?, ?]
  def tpe: Type[?]
  final override def optional: Primitive[A, Option[B]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+A, B] extends Primitive[A, B]:
    final override type Self[+a, b] = Primitive.Required[a, b]
    final override def imap[C](f: B => C)(g: C => B): Primitive.Required[A, C] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A, B](metadata: A, tpe: Type[B]) extends Primitive.Required[A, B]:
      override def update[C](f: A => C): Required[C, B] = copy(metadata = f(metadata))

    final case class Modify[A, B, C](primitive: Primitive.Required[A, B], f: B => C, g: C => B)
        extends Primitive.Required[A, C]:
      export primitive.{metadata, tpe}
      override def update[D](f: A => D): Required[D, C] = copy(primitive = primitive.update(f))

  sealed abstract class Optional[A, B] extends Primitive[A, B]:
    final override type Self[a, b] = Primitive[a, b]
    final override def imap[C](f: B => C)(g: C => B): Primitive.Optional[A, C] = Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Root[A, B](primitive: Primitive[A, B]) extends Primitive.Optional[A, Option[B]]:
      export primitive.{metadata, tpe}
      override def update[C](f: A => C): Primitive[C, Option[B]] = copy(primitive = primitive.update(f))

    final case class Modify[A, B, C](primitive: Primitive[A, B], f: B => C, g: C => B) extends Primitive.Optional[A, C]:
      export primitive.{metadata, tpe}
      override def update[D](f: A => D): Primitive[D, C] = copy(primitive = primitive.update(f))

abstract class Product[+A, B] extends Schema[A, B]:
  final override type Self[+a, b] = Product[a, b]
  final override type Optional[+a, b] = Product[a, b]
  final override def imap[C](f: B => C)(g: C => B): Product[A, C] = Product.Modify(this, f, g)
  final override def optional: Product[A, Option[B]] = Product.Optional(this)
  final def zipWith[C, D](metadata: C)(product: Product[?, D]): Product[C, (B, D)] =
    Product.Zip(metadata, this, product)

object Product:
  type Of[S <: Schema[?, ?], A, B] = Product[A, B] { type Of <: S }

  final case class Empty[A](metadata: A) extends Product[A, Unit]:
    override type Of = Nothing
    override def update[C](f: A => C): Product[C, Unit] = copy(metadata = f(metadata))

  final case class Modify[A, B, C](product: Product[A, B], f: B => C, g: C => B) extends Product[A, C]:
    export product.{metadata, Of}
    override def update[D](f: A => D): Product[D, C] = copy(product = product.update(f))

  final case class One[S <: Schema[?, B], A, B](metadata: A, schema: S) extends Product[A, B]:
    override type Of = S
    override def update[C](f: A => C): Product.Of[S, C, B] = copy(metadata = f(metadata))

  final case class Optional[A, B](product: Product[A, B]) extends Product[A, Option[B]]:
    export product.{metadata, Of}
    override def update[C](f: A => C): Product[C, Option[B]] = copy(product = product.update(f))

  final case class Zip[A, B, C](metadata: A, left: Product[?, B], right: Product[?, C]) extends Product[A, (B, C)]:
    override type Of = left.Of | right.Of
    override def update[D](f: A => D): Product[D, (B, C)] = copy(metadata = f(metadata))
