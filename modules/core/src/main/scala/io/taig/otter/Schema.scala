package io.taig.otter

import io.taig.otter.validation.Validation
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader

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
    override def validate[C](validation: Validation[B, ?, ?, C]): Collection.Reader[F, A, C] = ???

  sealed trait Writer[+F[_], +A, B] extends Schema.Writer[F, A, B]:
    override def contramap[C](f: C => B): Collection.Writer[F, A, C] = ???

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

    sealed trait Writer[-A] extends Primitive.Writer[A]:
      final override def contramap[C](f: C => A): Primitive.Required.Writer[C] = ???

  sealed trait Reader[+A] extends Schema.Reader[Nothing, Nothing, A]:
    override def validate[C](validation: Validation[A, ?, ?, C]): Primitive.Reader[C] = ???

  sealed trait Writer[-A] extends Schema.Writer[Nothing, Nothing, A]:
    override def contramap[C](f: C => A): Primitive.Writer[C] = ???

  final case class Root[A](tpe: Type[A]) extends Primitive[A]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
