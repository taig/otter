package io.taig.otter

import io.taig.otter.validation.Validation
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader
import cats.data.Validated.Valid

sealed trait Schema[+F[_], +A, B] extends Schema.Reader[F, A, B], Schema.Writer[F, A, B]:
  override def optional: Schema[F, A, Option[B]]
  def ivalidate[C](validation: Validation[B, ?, ?, C])(f: C => B): Schema[F, A, C]

object Schema:
  sealed trait Reader[+F[_], +A, +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def optional: Reader[F, A, Option[B]]
    def validate[C](validation: Validation[B, ?, ?, C]): Reader[F, A, C]

  sealed trait Writer[+F[_], +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Writer[F, A, C]
    def optional: Writer[F, A, Option[B]]

sealed trait Collection[+F[_], +A, B] extends Schema[F, A, B], Collection.Reader[F, A, B], Collection.Writer[F, A, B]:
  final override def optional: Collection[F, A, Option[B]] = Collection.Optional(this)
  final override def ivalidate[C](validation: Validation[B, ?, ?, C])(f: C => B): Collection[F, A, C] = ???

object Collection:
  sealed trait Reader[+F[_], +A, B] extends Schema.Reader[F, A, B]:
    override def optional: Collection.Reader[F, A, Option[B]] = ???
    override def validate[C](validation: Validation[B, ?, ?, C]): Collection.Reader[F, A, C] = ???

  object Reader:
    final case class Modify[F[_], A, B, C](self: Collection.Reader[F, A, B], validation: Validation[B, ?, ?, C])
        extends Collection.Reader[F, A, C]:
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[F[_], A, B](self: Collection.Reader[F, A, B]) extends Collection.Reader[F, A, Option[B]]:
      export self.constraints

    final case class Root[F[_], A <: F[Schema.Reader[F, ?, B]], B](schema: A)
        extends Collection.Reader[F, A, Vector[B]]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Writer[+F[_], +A, B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Collection.Writer[F, A, C] = ???
    override def optional: Collection.Writer[F, A, Option[B]] = ???

  final case class Modify[F[_], A, B, C](self: Collection[F, A, B], validation: Validation[B, ?, ?, C], f: C => B)
      extends Collection[F, A, C]:
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[F[_], A, B](self: Collection[F, A, B]) extends Collection[F, A, Option[B]]:
    export self.constraints

  final case class Root[F[_], A <: F[Schema[F, ?, B]], B](schema: A) extends Collection[F, A, Vector[B]]:
    override def constraints: Chain[Constraint[?]] = Chain.empty

sealed trait Primitive[A] extends Schema[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]:
  final override def optional: Primitive[Option[A]] = ???
  override def ivalidate[C](validation: Validation[A, ?, ?, C])(f: C => A): Primitive[C] = ???

object Primitive:
  sealed trait Required[A] extends Primitive[A], Primitive.Required.Reader[A], Primitive.Required.Writer[A]:
    final override def ivalidate[C](validation: Validation[A, ?, ?, C])(f: C => A): Primitive.Required[C] = ???

  object Required:
    sealed trait Reader[+A] extends Primitive.Reader[A]:
      final override def validate[C](validation: Validation[A, ?, ?, C]): Primitive.Required.Reader[C] = ???

    object Reader:
      final case class Modify[A, B](self: Primitive.Required.Reader[A], validation: Validation[A, ?, ?, B])
          extends Primitive.Required.Reader[B]:
        export self.tpe
        override def constraints: Chain[Constraint[?]] = validation.constraints

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      final override def contramap[C](f: C => A): Primitive.Required.Writer[C] = Writer.Modify(this, f)

    object Writer:
      final case class Modify[A, B](self: Primitive.Required.Writer[A], f: A => B) extends Primitive.Required.Writer[B]:
        export self.tpe

  sealed trait Reader[+A] extends Schema.Reader[Nothing, Nothing, A]:
    override def optional: Primitive.Reader[Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    override def validate[C](validation: Validation[A, ?, ?, C]): Primitive.Reader[C] = ???

  object Reader:
    final case class Modify[A, B](self: Primitive.Reader[A], validation: Validation[A, ?, ?, B])
        extends Primitive.Reader[B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[-A] extends Schema.Writer[Nothing, Nothing, A]:
    override def contramap[C](f: C => A): Primitive.Writer[C] = Writer.Modify(this, f)
    def tpe: Type[?]
    override def optional: Primitive.Writer[Option[A]] = Writer.Optional(this)

  object Writer:
    final case class Modify[A, B](self: Primitive.Writer[A], f: A => B) extends Primitive.Writer[B]:
      export self.tpe

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.tpe

  final case class Root[A](tpe: Type[A]) extends Primitive[A]:
    override def constraints: Chain[Constraint[?]] = Chain.empty

sealed trait Tuple[+F[_], +A, B] extends Schema[F, A, B], Tuple.Reader[F, A, B], Tuple.Writer[F, A, B]:
  final override def ivalidate[C](validation: Validation[B, ?, ?, C])(f: C => B): Tuple[F, A, C] = ???
  final override def optional: Tuple[F, A, Option[B]] = ???

object Tuple:
  sealed trait Reader[+F[_], +A, +B] extends Schema.Reader[F, A, B]:
    override def optional: Tuple.Reader[F, A, Option[B]] = ???
    final override def validate[C](validation: Validation[B, ?, ?, C]): Tuple.Reader[F, A, C] = ???

  sealed trait Writer[+F[_], +A, -B] extends Schema.Writer[F, A, B]:
    final override def contramap[C](f: C => B): Tuple.Writer[F, A, C] = ???
    override def optional: Tuple.Writer[F, A, Option[B]] = ???
