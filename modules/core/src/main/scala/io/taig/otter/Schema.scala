package io.taig.otter

sealed abstract class Schema[A] extends Schema.Ops[Schema, Schema, A]:
  type Of <: Schema[?]

object Schema:
  type Of[S <: Schema[?], A] = Schema[A] { type Of <: S }

  trait Ops[+S[a], +O[a], A]:
    def imap[B](f: A => B)(g: B => A): S[B]
    final def const(value: A): S[Unit] = imap(_ => ())(_ => value)
    def optional: O[Option[A]]

sealed abstract class Value[A] extends Schema[A] with Value.Ops[Value, Value, A]

object Value:
  type Of[S <: Schema[?], A] = Value[A] { type Of <: S }

  trait Ops[+S[a], +O[a], A] extends Schema.Ops[S, O, A]

sealed abstract class Primitive[A] extends Value[A] with Primitive.Ops[Primitive, Primitive, A]:
  self =>
  final override type Of = Primitive[?]
  def tpe: Type[?]
  final override def optional: Primitive[Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  trait Ops[+S[a], +O[a], A] extends Value.Ops[S, O, A]

  sealed abstract class Required[A] extends Primitive[A] with Primitive.Ops[Primitive.Required, Primitive, A]:
    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[A] extends Primitive[A]:
    final override def imap[B](f: A => B)(g: B => A): Primitive[B] = Primitive.Optional.Modify(this, f, g)

  object Optional:
    final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export primitive.tpe

    final case class Modify[A, B](primitive: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]:
      export primitive.tpe

abstract class Tuple[A] extends Schema[A] with Tuple.Ops[Tuple, Tuple, A]:
  final override def imap[B](f: A => B)(g: B => A): Tuple[B] = ???
  final override def optional: Tuple[Option[A]] = ???
  final override def product[B](tuple: Tuple[B]): Tuple[(A, B)] = ???

object Tuple:
  type Of[S <: Schema[?], A] = Tuple[A] { type Of <: S }

  trait Ops[S[a], +O[a], A] extends Schema.Ops[S, O, A]:
    def product[B](tuple: S[B]): S[(A, B)]

  case object Empty extends Tuple[Unit]:
    override type Of = Nothing

//   case class Empty[M](metadata: M) extends Tuple[M, Unit]:
//     override type Of = Nothing
//     override def update[N](f: M => N): Tuple[N, Unit] = copy(metadata = f(metadata))

//   final case class Modify[M, A, B](product: Tuple[M, A], f: A => B, g: B => A) extends Tuple[M, B]:
//     export product.{metadata, Of}
//     override def update[N](f: M => N): Tuple[N, B] = copy(product = product.update(f))

//   final case class One[S <: Schema[?, A], M, A](metadata: M, schema: S) extends Tuple[M, A]:
//     override type Of = S
//     override def update[N](f: M => N): Tuple[N, A] = copy(metadata = f(metadata))

//   final case class Optional[M, A](product: Tuple[M, A]) extends Tuple[M, Option[A]]:
//     export product.{metadata, Of}
//     override def update[N](f: M => N): Tuple[N, Option[A]] = copy(product = product.update(f))

//   final case class Product[M, A, B](metadata: M, left: Tuple[?, A], right: Tuple[?, B]) extends Tuple[M, (A, B)]:
//     override type Of = left.Of | right.Of
//     override def update[N](f: M => N): Tuple[N, (A, B)] = copy(metadata = f(metadata))
