package io.taig.otter

import io.taig.otter.validation.Validation
import cats.data.Chain

sealed abstract class Schema[+M, A]:
  type Self[+m, a] <: Schema[m, a]
  type Optional[+m, a] <: Schema[m, a]

  def metadata: M
  def update[N](f: M => N): Self[N, A]

  def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(g: C => A): Self[M, C]
  final def ivalidate[B](validation: Validation[A, A, B])(g: B => A): Self[M, B] = ivalidate(this)(validation)(g)
  final def validate(validation: Validation[A, A, Unit]): Self[M, A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[M, B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[M, Unit] = imap(_ => ())(_ => value)

  def optional: Optional[M, Option[A]]

sealed abstract class Collection[+M, A] extends Schema[M, A]:
  final override type Self[+m, a] = Collection.Of[Of, m, a]
  final override type Optional[+m, a] = Collection.Of[Of, m, a]
  type Of <: Schema[?, ?]

  final override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(
      g: C => A
  ): Collection.Of[Of, M, C] = Collection.Validate(this, constraint, validation, g)
  final override def optional: Collection.Of[Of, M, Option[A]] = Collection.Optional(this)

object Collection:
  type Of[S <: Schema[?, ?], +M, A] = Collection[M, A] { type Of = S }

  final case class Optional[S <: Schema[?, ?], M, A](schema: Collection.Of[S, M, A]) extends Collection[M, Option[A]]:
    export schema.{metadata, Of}
    override def update[N](f: M => N): Collection.Of[S, N, Option[A]] = copy(schema = schema.update(f))

  final case class Root[S <: Schema[?, ?], M, A](metadata: M, schema: Schema[?, A]) extends Collection[M, Chain[A]]:
    override type Of = S
    override def update[N](f: M => N): Collection.Of[S, N, Chain[A]] = copy(metadata = f(metadata))

  final case class Validate[S <: Schema[?, ?], M, A, B, C](
      schema: Collection.Of[S, M, A],
      constraint: Schema[?, B],
      validation: Validation[A, B, C],
      g: C => A
  ) extends Collection[M, C]:
    export schema.{metadata, Of}
    override def update[N](f: M => N): Collection.Of[S, N, C] = copy(schema = schema.update(f))

sealed abstract class Primitive[+M, A] extends Schema[M, A]:
  override type Self[+m, a] <: Primitive[m, a]
  final override type Optional[+m, a] = Primitive[m, a]
  def tpe: Type[?]
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+M, A] extends Primitive[M, A]:
    final override type Self[+m, a] = Primitive.Required[m, a]
    final override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive.Required[M, C] = Required.Validate(this, constraint, validation, g)

  object Required:
    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]:
      override def update[N](f: M => N): Primitive.Required[N, A] = copy(metadata = f(metadata))

    final case class Validate[M, A, B, C](
        schema: Primitive.Required[M, A],
        constraint: Schema[?, B],
        validation: Validation[A, B, C],
        g: C => A
    ) extends Primitive.Required[M, C]:
      export schema.{metadata, tpe}
      override def update[N](f: M => N): Required[N, C] = copy(schema = schema.update(f))

  sealed abstract class Optional[+M, A] extends Primitive[M, A]:
    final override type Self[+m, a] = Primitive[m, a]
    final override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive[M, C] = ???

  object Optional:
    final case class Root[M, A](schema: Primitive[M, A]) extends Primitive.Optional[M, Option[A]]:
      export schema.{metadata, tpe}
      override def update[N](f: M => N): Primitive[N, Option[A]] = copy(schema = schema.update(f))

    final case class Validate[M, A, B, C](
        schema: Primitive[M, A],
        constraint: Schema[?, B],
        validation: Validation[A, B, C],
        g: C => A
    ) extends Primitive.Optional[M, C]:
      export schema.{metadata, tpe}
      override def update[N](f: M => N): Primitive[N, C] = copy(schema = schema.update(f))

sealed abstract class Tuple[+M, A] extends Schema[M, A]:
  final override type Self[+m, a] = Tuple[m, a]
  final override type Optional[+m, a] = Tuple[m, a]
  type Of <: Schema[?, ?]

  final def productWith[O, S <: Schema[?, ?], N, B](tuple: Tuple.Of[S, N, B])(
      f: (M, N) => O
  ): Tuple.Of[Of | S, O, (A, B)] = ??? // Tuple.Product(this, tuple)
  final override def optional: Tuple.Of[Of, M, Option[A]] = ??? // Tuple.Optional(this)
  final override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(
      g: C => A
  ): Tuple.Of[Of, M, C] = ???

object Tuple:
  type Of[S <: Schema[?, ?], +M, A] = Tuple[M, A] { type Of = S }
//   case object Empty extends Tuple[Nothing, Unit]:
//     override val size: Int = 0

//   final case class One[S[_], A](schema: S[A]) extends Tuple[S[A], A]:
//     override def size: Int = 1

//   final case class Optional[S, A](schema: Tuple[S, A]) extends Tuple[S, Option[A]]:
//     export schema.size

//   final case class Product[S, A, T, B](left: Tuple[S, A], right: Tuple[T, B]) extends Tuple[S | T, (A, B)]:
//     override def size: Int = left.size + right.size

//   final case class Validate[S, A, B, C](
//       schema: Tuple[S, A],
//       constraint: Schema[B],
//       validation: Validation[A, B, C],
//       g: C => A
//   ) extends Tuple[S, C]:
//     export schema.size

// sealed abstract class Union[+S, A] extends Schema[A] with Operation.Union[Union, Schema, Tuple, S, A]:
//   final override def asSelf: Union[S, A] = this
//   override def orElse[T, B](union: Union[T, B]): Union[S | T, Either[A, B]] = Union.OrElse(this, union)
//   override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(g: C => A): Union[S, C] = ???
//   override def optional: Union[S, Option[A]] = Union.Optional(this)
//   override def toTuple: Tuple[Union[S, A], A] = Tuple.One(this)

// object Union:
//   final case class One[S[_], A](schema: S[A]) extends Union[S[A], A]

//   final case class OrElse[S, A, T, B](left: Union[S, A], right: Union[T, B]) extends Union[S | T, A + B]

//   final case class Optional[S, A](schema: Union[S, A]) extends Union[S, Option[A]]

//   final case class Validate[S, A, B, C](
//       schema: Union[S, A],
//       constraint: Schema[B],
//       validation: Validation[A, B, C],
//       g: C => A
//   ) extends Union[S, C]
