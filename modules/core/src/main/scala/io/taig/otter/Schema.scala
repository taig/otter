package io.taig.otter

import io.taig.otter.validation.Validation

sealed abstract class Schema[A] extends Schema.Ops[Schema, Schema, Tuple, A]:
  type Of <: Schema[?]
  override def toTuple: Tuple[A] = ???

object Schema:
  type Of[S <: Schema[?], M, A] = Schema[A] { type Of <: S }

  trait Ops[+Self[a], +Optional[a], +Tuple[a], A]:
    def imap[B](f: A => B)(g: B => A): Self[B]
    final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)
    def optional: Optional[Option[A]]
    def toTuple: Tuple[A]

sealed abstract class Value[A] extends Schema[A] with Value.Ops[Value, Value, Tuple, A]

object Value:
  type Of[S <: Schema[?], A] = Value[A] { type Of <: S }

  trait Ops[+Self[_], +Optional[_], +Tuple[_], A] extends Schema.Ops[Self, Optional, Tuple, A]

sealed abstract class Primitive[A] extends Value[A] with Primitive.Ops[Primitive, Primitive, Tuple, A]:
  self =>
  final override type Of = Primitive[?]

object Primitive:
  trait Ops[+Self[_], +Optional[_], +Tuple[_], A] extends Value.Ops[Self, Optional, Tuple, A]:
    def tpe: Type[?]

  sealed abstract class Required[A] extends Primitive[A] with Primitive.Ops[Primitive.Required, Primitive, Tuple, A]:
    override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = Primitive.Required.Modify(this, f, g)
    override def optional: Primitive.Required[Option[A]] = ???

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]:
      export primitive.tpe

    final case class Validate[A, C, B](
        primitive: Primitive.Required[A],
        constraint: Schema[C],
        validation: Validation[A, C, B],
        g: B => A
    ) extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[A] extends Primitive[A] with Primitive.Ops[Primitive, Primitive, Tuple, A]:
    final override def imap[B](f: A => B)(g: B => A): Primitive[B] = Primitive.Optional.Modify(this, f, g)
    final override def optional: Primitive[Option[A]] = Optional.Root(this)

  object Optional:
    final case class Root[A](schema: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export schema.tpe

    final case class Modify[A, B](schema: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]:
      export schema.tpe

    final case class Validate[C, A, B](
        schema: Primitive[A],
        constraint: Schema[C],
        validation: Validation[A, C, B],
        g: B => A
    ) extends Primitive.Optional[B]:
      export schema.tpe

abstract class Tuple[A] extends Schema[A] with Tuple.Ops[Tuple, A]:
  override def product[B](tuple: Tuple[B]): Tuple[(A, B)] = ???
  override def imap[B](f: A => B)(g: B => A): Tuple[B] = ???
  override def optional: Tuple[Option[A]] = ???

object Tuple:
  type Of[S <: Schema[?], A] = Tuple[A] { type Of <: S }

  trait Ops[Self[_], A] extends Schema.Ops[Self, Self, Self, A]:
    def size: Int
    def product[B](tuple: Self[B]): Self[(A, B)]

  case object Empty extends Tuple[Unit]:
    override type Of = Nothing
    override def size: Int = 0

  final case class Modify[A, B](schema: Tuple[A], f: A => B, g: B => A) extends Tuple[B]:
    export schema.{size, Of}

  final case class One[S <: Schema[A], A](schema: S) extends Tuple[A]:
    override type Of = S
    override def size: Int = 1

  final case class Optional[A](schema: Tuple[A]) extends Tuple[Option[A]]:
    export schema.{size, Of}

  final case class Product[A, B](left: Tuple[A], right: Tuple[B]) extends Tuple[(A, B)]:
    override type Of = left.Of | right.Of
    override def size: Int = left.size + right.size
