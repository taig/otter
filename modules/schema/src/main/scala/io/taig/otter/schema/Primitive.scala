package io.taig.otter.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def format: Option[String]
  def format(f: Option[String] => Option[String]): Primitive[A]
  final def format(value: Option[String]): Primitive[A] = format(_ => value)
  final def format(value: String): Primitive[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = ???

object Primitive:
  final case class Root[A](description: Option[String], example: Option[A], format: Option[String], tpe: Type[A])
      extends Primitive[A]:
    override def isOptional: Boolean = false
    override def constraints: Chain[Constraint] = Chain.empty
    override def description(f: Option[String] => Option[String]): Primitive[A] = copy(description = f(description))
    override def example(f: Option[A] => Option[A]): Primitive[A] = copy(example = f(example))
    override def format(f: Option[String] => Option[String]): Primitive[A] = copy(format = f(format))

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def example: Option[Option[A]] = self.example.map(_.some)
    override def format: Option[String] = self.format
    override def description: Option[String] = self.description
    override def description(f: Option[String] => Option[String]): Primitive[Option[A]] =
      copy(self = self.description(f))
    override def example(f: Option[Option[A]] => Option[Option[A]]): Primitive[Option[A]] =
      copy(self = self.example(fa => f(fa.map(_.some)).flatten))
    override def format(f: Option[String] => Option[String]): Primitive[Option[A]] = copy(self = self.format(f))

  final case class Validate[A, B](self: Primitive[A], validation: Validation[A, B], g: B => A) extends Primitive[B]:
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def example: Option[B] = self.example.flatMap(validation(_).toOption)
    override def format: Option[String] = self.format
    override def description: Option[String] = self.description
    override def format(f: Option[String] => Option[String]): Primitive[B] = copy(self = self.format(f))
    override def description(f: Option[String] => Option[String]): Primitive[B] =
      copy(self = self.description(f))
    override def example(f: Option[B] => Option[B]): Primitive[B] =
      copy(self = self.example(fa => fa.traverse(validation(_).toOption).flatten.map(g)))
