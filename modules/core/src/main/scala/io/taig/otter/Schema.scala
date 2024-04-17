package io.taig.otter

import io.taig.otter.validation.Validation
import cats.data.Chain

sealed abstract class Schema[A] extends Operation[Schema, Schema, Schema, Tuple, A]

sealed abstract class Value[A] extends Schema[A] with Operation.Value[Value, Value, Schema, Tuple, A]

sealed abstract class Collection[+S, A] extends Schema[A] with Operation.Collection[Collection, Schema, Tuple, S, A]:
  override def asSelf: Collection[S, A] = this
  override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(g: C => A): Collection[S, C] =
    Collection.Validate(this, constraint, validation, g)
  override def optional: Collection[S, Option[A]] = Collection.Optional(this)
  override def toTuple: Tuple[Collection[S, A], A] = Tuple.One(this)

object Collection:
  final case class Optional[S, A](schema: Collection[S, A]) extends Collection[S, Option[A]]
  final case class Root[S[_], A](schema: S[A]) extends Collection[S[A], Chain[A]]
  final case class Validate[S, A, B, C](
      schema: Collection[S, A],
      constraint: Schema[B],
      validation: Validation[A, B, C],
      g: C => A
  ) extends Collection[S, C]

sealed abstract class Primitive[A] extends Value[A] with Operation.Primitive[Primitive, Primitive, Schema, Tuple, A]

object Primitive:
  sealed abstract class Required[A]
      extends Primitive[A]
      with Operation.Primitive[Primitive.Required, Primitive, Schema, Tuple, A]:
    final override def asSelf: Primitive.Required[A] = this
    final override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive.Required[C] = Required.Validate(this, constraint, validation, g)
    final override def optional: Primitive[Option[A]] = Primitive.Optional.Root(this)
    final override def toTuple: Tuple[Primitive.Required[A], A] = Tuple.One(this)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Validate[A, B, C](
        schema: Primitive.Required[A],
        constraint: Schema[B],
        validation: Validation[A, B, C],
        g: C => A
    ) extends Primitive.Required[C]:
      export schema.tpe

  sealed abstract class Optional[A]
      extends Primitive[A]
      with Operation.Primitive[Primitive, Primitive, Schema, Tuple, A]:
    final override def asSelf: Primitive[A] = this
    final override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive[C] = Optional.Validate(this, constraint, validation, g)
    final override def optional: Primitive[Option[A]] = Optional.Root(this)
    final override def toTuple: Tuple[Primitive[A], A] = Tuple.One(this)

  object Optional:
    final case class Root[S, A](schema: Primitive[A]) extends Primitive.Optional[Option[A]]:
      export schema.tpe

    final case class Validate[A, B, C](
        schema: Primitive[A],
        constraint: Schema[B],
        validation: Validation[A, B, C],
        g: C => A
    ) extends Primitive.Optional[C]:
      export schema.tpe

sealed abstract class Tuple[+S, A] extends Schema[A] with Operation.Tuple[Tuple, Schema, S, A]:
  final override def asSelf: Tuple[S, A] = this
  final override def product[T, B](tuple: Tuple[T, B]): Tuple[S | T, (A, B)] = Tuple.Product(this, tuple)
  final override def optional: Tuple[S, Option[A]] = Tuple.Optional(this)
  final override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
      g: C => A
  ): Tuple[S, C] = Tuple.Validate(this, constraint, validation, g)
  final override def toTuple: Tuple[Tuple[S, A], A] = Tuple.One(this)

object Tuple:
  case object Empty extends Tuple[Nothing, Unit]:
    override val size: Int = 0

  final case class One[S[_], A](schema: S[A]) extends Tuple[S[A], A]:
    override def size: Int = 1

  final case class Optional[S, A](schema: Tuple[S, A]) extends Tuple[S, Option[A]]:
    export schema.size

  final case class Product[S, A, T, B](left: Tuple[S, A], right: Tuple[T, B]) extends Tuple[S | T, (A, B)]:
    override def size: Int = left.size + right.size

  final case class Validate[S, A, B, C](
      schema: Tuple[S, A],
      constraint: Schema[B],
      validation: Validation[A, B, C],
      g: C => A
  ) extends Tuple[S, C]:
    export schema.size

sealed abstract class Union[+S, A] extends Schema[A] with Operation.Union[Union, Schema, Tuple, S, A]:
  final override def asSelf: Union[S, A] = this
  override def orElse[T, B](union: Union[T, B]): Union[S | T, Either[A, B]] = Union.OrElse(this, union)
  override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(g: C => A): Union[S, C] = ???
  override def optional: Union[S, Option[A]] = Union.Optional(this)
  override def toTuple: Tuple[Union[S, A], A] = Tuple.One(this)

object Union:
  final case class One[S[_], A](schema: S[A]) extends Union[S[A], A]

  final case class OrElse[S, A, T, B](left: Union[S, A], right: Union[T, B]) extends Union[S | T, A + B]

  final case class Optional[S, A](schema: Union[S, A]) extends Union[S, Option[A]]

  final case class Validate[S, A, B, C](
      schema: Union[S, A],
      constraint: Schema[B],
      validation: Validation[A, B, C],
      g: C => A
  ) extends Union[S, C]
