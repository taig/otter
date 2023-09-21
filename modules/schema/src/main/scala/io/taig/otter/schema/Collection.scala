package io.taig.otter.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation}

sealed abstract class Collection[F[a] <: Schema[a], A] extends Schema[A]:
  override type Self[a] = Collection[F, a]

  final override def optional: Collection[F, Option[A]] = ???

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[F, B] = ???

object Collection:
  final case class Root[F[a] <: Schema[a], A](description: Option[String], example: Option[Chain[A]], schema: F[A])
      extends Collection[F, Chain[A]]:
    override def isOptional: Boolean = false
    override def constraints: Chain[Constraint] = Chain.empty
    override def description(f: Option[String] => Option[String]): Self[Chain[A]] = copy(description = f(description))
    override def example(f: Option[Chain[A]] => Option[Chain[A]]): Self[Chain[A]] = copy(example = f(example))

  final case class Optional[F[a] <: Schema[a], A](self: Collection[F, A]) extends Collection[F, Option[A]]:
    override def isOptional: Boolean = true
    override def constraints: Chain[Constraint] = self.constraints
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Collection[F, Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Collection[F, Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[F[a] <: Schema[a], A, B](self: Collection[F, A], validation: Validation[A, B], g: B => A)
      extends Collection[F, B]:
    override def isOptional: Boolean = self.isOptional
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Collection[F, B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Collection[F, B] =
      copy(self = self.example(fa => fa.traverse(validation(_).toOption).flatten.map(g)))
