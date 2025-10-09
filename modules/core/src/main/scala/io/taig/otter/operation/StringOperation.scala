package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.otter.InvariantK

trait StringOperation[Self[_]]:
  def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String]

  def parser[A](name: String, decode: String => Either[String, A], encode: A => String): Self[A]

  def constraints[A](self: Self[A]): Chain[Constraint.Primitive.Text]

object StringOperation:
  inline def apply[Self[_]](using operation: StringOperation[Self]): StringOperation[Self] = operation

  given InvariantK[StringOperation] with
    extension [G[_]](operation: StringOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): StringOperation[H] =
        new StringOperation[H]:
          override def string(validation: Validation[Constraint.Primitive.Text, String]): H[String] =
            fK(operation.string(validation))

          override def parser[A](name: String, decode: String => Either[String, A], encode: A => String): H[A] =
            fK(operation.parser(name, decode, encode))

          override def constraints[A](self: H[A]): Chain[Constraint.Primitive.Text] =
            operation.constraints(gK(self))
