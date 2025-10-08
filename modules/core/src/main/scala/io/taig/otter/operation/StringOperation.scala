package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.otter.OperationInvariant

trait StringOperation[Self[_]]:
  def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String]

  def parser[A](name: String, decode: String => Either[String, A], encode: A => String): Self[A]

  def constraints[A](self: Self[A]): Chain[Constraint.Primitive.Text]

object StringOperation:
  inline def apply[Self[_]](using operation: StringOperation[Self]): StringOperation[Self] = operation

  given OperationInvariant[[Shape[_], Self[_[a] <: Shape[a], _]] =>> StringOperation[[a] =>> Self[Nothing, a]]] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: StringOperation[[a] =>> Self[Nothing, a]])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): StringOperation[[a] =>> T[Nothing, a]] = new StringOperation[[a] =>> T[Nothing, a]]:
        override def string(validation: Validation[Constraint.Primitive.Text, String]): T[Nothing, String] = fK(
          operation.string(validation)
        )

        override def parser[A](name: String, decode: String => Either[String, A], encode: A => String): T[Nothing, A] =
          fK(operation.parser(name, decode, encode))

        override def constraints[A](self: T[Nothing, A]): Chain[Constraint.Primitive.Text] =
          operation.constraints(gK(self))
