package io.taig.otter.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation}

import scala.collection.immutable.VectorMap

sealed abstract class Dictionary[A] extends Schema[A]:
  final override type Self[a] = Dictionary[a]

  final override def optional: Dictionary[Option[A]] = Dictionary.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    Dictionary.Validate(this, validation, g)

object Dictionary:
  final case class Root[A, B](
      description: Option[String],
      example: Option[VectorMap[A, B]],
      key: Schema.Value[A],
      value: Schema[B]
  ) extends Dictionary[VectorMap[A, B]]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description(f: Option[String] => Option[String]): Dictionary[VectorMap[A, B]] =
      copy(description = f(description))
    override def example(f: Option[VectorMap[A, B]] => Option[VectorMap[A, B]]): Dictionary[VectorMap[A, B]] =
      copy(example = f(example))

  final case class Optional[A](self: Dictionary[A]) extends Dictionary[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Dictionary[Option[A]] =
      copy(self = self.description(f))
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def example(f: Option[Option[A]] => Option[Option[A]]): Dictionary[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))

  final case class Validate[A, B](self: Dictionary[A], validation: Validation[A, B], g: B => A) extends Dictionary[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Dictionary[B] = copy(self = self.description(f))
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def example(f: Option[B] => Option[B]): Dictionary[B] =
      copy(self = self.example(fa => f(fa.flatMap(validation(_).toOption)).map(g)))
