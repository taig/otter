package io.taig.otter

import io.taig.otter.validation.Validation

sealed abstract class Schema[A] extends Operation[Schema, Schema, Schema, Tuple, A]:
  type Of <: Schema[?]
  override def toTuple: Tuple[A] = Tuple.One(this)

object Schema:
  type Of[S <: Schema[?], M, A] = Schema[A] { type Of <: S }

sealed abstract class Value[A] extends Schema[A] with Operation.Value[Value, Value, Schema, Tuple, A]

object Value:
  type Of[S <: Schema[?], A] = Value[A] { type Of <: S }

sealed abstract class Primitive[A] extends Value[A] with Operation.Primitive[Primitive, Primitive, Schema, Tuple, A]:
  self =>
  final override type Of = Primitive[?]

object Primitive:
  sealed abstract class Required[A]
      extends Primitive[A]
      with Operation.Primitive[Primitive.Required, Primitive, Schema, Tuple, A]:
    override def asSelf: Primitive.Required[A] = this
    override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive.Required[C] = Required.Validate(this, constraint, validation, g)
    override def optional: Primitive[Option[A]] = Primitive.Optional.Root(this)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]:
      export primitive.tpe

    final case class Validate[A, C, B](
        schema: Primitive.Required[A],
        constraint: Schema[C],
        validation: Validation[A, C, B],
        g: B => A
    ) extends Primitive.Required[B]:
      export schema.tpe

  sealed abstract class Optional[A]
      extends Primitive[A]
      with Operation.Primitive[Primitive, Primitive, Schema, Tuple, A]:
    final override def asSelf: Primitive[A] = this
    final override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive[C] = Optional.Validate(this, constraint, validation, g)
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

abstract class Tuple[A] extends Schema[A] with Operation.Tuple[Tuple, Schema, A]:
  final override def asSelf: Tuple[A] = this
  final override def product[B](tuple: Tuple[B]): Tuple[(A, B)] = Tuple.Product(this, tuple)
  final override def optional: Tuple[Option[A]] = Tuple.Optional(this)
  final override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(g: C => A): Tuple[C] = ???

object Tuple:
  type Of[S <: Schema[?], A] = Tuple[A] { type Of <: S }

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
