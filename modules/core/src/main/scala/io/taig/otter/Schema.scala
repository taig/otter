package io.taig.otter

sealed abstract class Schema[+M, A]:
  type Self[+m, a] <: Schema[m, a]
  type Optional[+m, a] <: Schema[m, a]
  type Of <: Schema[?, ?]

  def metadata: M
  def update[N](f: M => N): Self[N, A]
  def imap[B](f: A => B)(g: B => A): Self[M, B]
  final def const(value: A): Self[M, Unit] = imap(_ => ())(_ => value)
  def optional: Optional[M, Option[A]]

object Schema:
  type Of[S <: Schema[?, ?], M, A] = Schema[M, A] { type Of <: S }

sealed abstract class Value[+M, A] extends Schema[M, A]:
  override type Self[+m, a] <: Value[m, a]
  override type Optional[+m, a] <: Value[m, a]

object Value:
  type Of[S <: Schema[?, ?], M, A] = Value[M, A] { type Of <: S }

sealed abstract class Primitive[+M, A] extends Value[M, A]:
  self =>
  override type Self[+m, a] <: Primitive[m, a]
  final override type Optional[+m, a] = Primitive[m, a]
  final override type Of = Primitive[?, ?]
  def tpe: Type[?]
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+M, A] extends Primitive[M, A]:
    override type Self[+m, a] = Primitive.Required[m, a]
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[M, B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]:
      override def update[N](f: M => N): Required[N, A] = copy(metadata = f(metadata))

    final case class Modify[M, A, B](primitive: Primitive.Required[M, A], f: A => B, g: B => A)
        extends Primitive.Required[M, B]:
      export primitive.{metadata, tpe}
      override def update[N](f: M => N): Primitive.Required[N, B] = copy(primitive = primitive.update(f))

  sealed abstract class Optional[M, A] extends Primitive[M, A]:
    final override type Self[+m, a] = Primitive[m, a]
    final override def imap[B](f: A => B)(g: B => A): Primitive[M, B] = Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Root[M, A](schema: Primitive[M, A]) extends Primitive.Optional[M, Option[A]]:
      export schema.{metadata, tpe}
      override def update[N](f: M => N): Primitive[N, Option[A]] = copy(schema = schema.update(f))

    final case class Modify[M, A, B](schema: Primitive[M, A], f: A => B, g: B => A) extends Primitive.Optional[M, B]:
      export schema.{metadata, tpe}
      override def update[N](f: M => N): Primitive[N, B] = copy(schema = schema.update(f))

abstract class Tuple[+M, A] extends Schema[M, A]:
  final override type Self[+m, a] = Tuple[m, a]
  final override type Optional[+m, a] = Tuple[m, a]
  final override def imap[B](f: A => B)(g: B => A): Tuple[M, B] = Tuple.Modify(this, f, g)
  final override def optional: Tuple[M, Option[A]] = Tuple.Optional(this)
  final def productWith[N, O, B](f: (M, N) => O)(tuple: Tuple[N, B]): Tuple[O, (A, B)] =
    Tuple.Product(f(metadata, tuple.metadata), this, tuple)

object Tuple:
  type Of[S <: Schema[?, ?], +M, A] = Tuple[M, A] { type Of <: S }

  final case class Empty[M](metadata: M) extends Tuple[M, Unit]:
    override type Of = Nothing
    override def update[N](f: M => N): Tuple[N, Unit] = copy(metadata = f(metadata))

  final case class Modify[M, A, B](schema: Tuple[M, A], f: A => B, g: B => A) extends Tuple[M, B]:
    export schema.{metadata, Of}
    override def update[N](f: M => N): Tuple[N, B] = copy(schema = schema.update(f))

  final case class One[S <: Schema[?, A], M, A](metadata: M, schema: S) extends Tuple[M, A]:
    override type Of = S
    override def update[N](f: M => N): Tuple[N, A] = copy(metadata = f(metadata))

  final case class Optional[M, A](schema: Tuple[M, A]) extends Tuple[M, Option[A]]:
    export schema.{metadata, Of}
    override def update[N](f: M => N): Tuple[N, Option[A]] = copy(schema = schema.update(f))

  final case class Product[M, A, B](metadata: M, left: Tuple[?, A], right: Tuple[?, B]) extends Tuple[M, (A, B)]:
    override type Of = left.Of | right.Of
    override def update[N](f: M => N): Tuple[N, (A, B)] = copy(metadata = f(metadata))
