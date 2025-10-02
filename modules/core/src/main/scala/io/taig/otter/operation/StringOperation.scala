package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.validation.Validation

trait StringOperation[Self[_]]:
  self =>

  def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String]

  def parser[A](name: String, decode: String => Either[String, A], encode: A => String): Self[A]

  extension [A](self: Self[A]) def constraints: Chain[Constraint]

  def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): StringOperation[G] = new StringOperation[G]:
    override def string(validation: Validation[Constraint.Primitive.Text, String]): G[String] =
      fK(self.string(validation))

    override def parser[A](name: String, decode: String => Either[String, A], encode: A => String): G[A] =
      fK(self.parser(name, decode, encode))

    extension [A](ga: G[A]) override def constraints: Chain[Constraint] = self.constraints(gK(ga))

object StringOperation:
  inline def apply[Self[_]](using operation: StringOperation[Self]): StringOperation[Self] = operation

  given InvariantK[StringOperation] with
    extension [G[_]](self: StringOperation[G])
      def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): StringOperation[H] =
        self.imapK(fK)(gK)
