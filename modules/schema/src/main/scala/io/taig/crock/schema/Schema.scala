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
          copy: schema.type => Self[A]
      ): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = property(schema).value
        override def modify(f: Option[C] => Option[C]): Self[A] = copy(schema)

      def apply[B, C, D](
          schema: Schema[B],
          property: schema.type => schema.Property.Optional[B],
          copy: schema.type => Self[A],
          validation: Validation[B, D],
          g: D => B
      ): Property.Optional[D] = new Optional[D]:
        override def value: Option[D] = property(schema).value.flatMap(validation(_).toOption)
        override def modify(f: Option[D] => Option[D]): Self[A] =
          copy(schema)

  def description: Property.Optional[String]
  def example: Property.Optional[A]

  def imap[B](f: A => B)(g: B => A): Self[B]
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

object Schema:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
