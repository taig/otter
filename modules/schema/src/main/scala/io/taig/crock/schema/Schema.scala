package io.taig.crock.schema

import cats.data.Chain
import io.taig.crock.validation.*

import scala.annotation.targetName

abstract class Schema[A]:
  self =>

  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }

  def constraints: Chain[Constraint]

  abstract class Property[B]:
    def value: B
    def modify(f: B => B): Self[A]
    final def apply(b: B): Self[A] = modify(_ => b)

  object Property:
    abstract class Optional[B] extends Property[Option[B]]:
      @targetName("as")
      final def apply(b: B): Self[A] = apply(Some(b))
      final def clear: Self[A] = apply(None)

    object Optional:
      def apply[B](b: Option[B], g: (Option[B] => Option[B]) => Self[A]): Property.Optional[B] = new Optional[B]:
        override def value: Option[B] = b
        override def modify(f: Option[B] => Option[B]): Self[A] = g(f)

      def apply[B, C](
          schema: Schema[B],
          property: schema.type => schema.Property.Optional[C],
          copy: Option[C] => Self[A]
      ): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = property(schema).value
        override def modify(f: Option[C] => Option[C]): Self[A] = copy(f(property(schema).value))

      def apply[B, C](
          schema: Schema[B],
          property: schema.type => schema.Property.Optional[B],
          copy: Option[B] => Self[A],
          validation: Validation[B, C],
          g: C => B
      ): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = property(schema).value.flatMap(validation(_).toOption)
        override def modify(f: Option[C] => Option[C]): Self[A] =
          copy(f(property(schema).value.flatMap(validation(_).toOption)).map(g))

  def description: Property.Optional[String]
  def example: Property.Optional[A]

  def optional: Self[Option[A]]
  def isOptional: Boolean

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

object Schema:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
