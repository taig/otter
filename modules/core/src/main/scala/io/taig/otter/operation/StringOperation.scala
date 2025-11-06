package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.validation.Validation

trait StringOperation[F[_]]:
  self =>

  def constraints[A](self: F[A]): Chain[Constraint.Primitive.Text]

  def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A]

  def string(validation: Validation[Constraint.Primitive.Text, String]): F[String]

  def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation[G] = new StringOperation[G]:
    override def constraints[A](ta: G[A]): Chain[Constraint.Primitive.Text] = self.constraints(gK(ta))

    override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): G[A] =
      fK(self.codec(name, decode, encode))

    override def string(validation: Validation[Constraint.Primitive.Text, String]): G[String] =
      fK(self.string(validation))

object StringOperation:
  trait Read[F[_]] extends StringOperation[F]:
    self =>

    final override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A] =
      parser(name, decode)

    def parser[A](name: String, decode: String => Either[String, A]): F[A]

    override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation.Read[G] = new Read[G]:
      override def constraints[A](ga: G[A]): Chain[Constraint.Primitive.Text] = self.constraints(gK(ga))

      override def parser[A](name: String, decode: String => Either[String, A]): G[A] = fK(self.parser(name, decode))

      override def string(validation: Validation[Constraint.Primitive.Text, String]): G[String] =
        fK(self.string(validation))

  object Read:
    inline def apply[F[_]](using self: StringOperation.Read[F]): StringOperation.Read[F] = self

    given InvariantK[StringOperation.Read] with
      extension [F[_]](self: StringOperation.Read[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation.Read[G] =
          self.imapK(fK)(gK)

  trait Write[F[_]] extends StringOperation[F]:
    self =>

    final override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A] =
      printer(name, encode)

    final override def constraints[A](self: F[A]): Chain[Constraint.Primitive.Text] = Chain.empty

    def printer[A](name: String, encode: A => String): F[A]

    def string: F[String]

    final override def string(validation: Validation[Constraint.Primitive.Text, String]): F[String] = string

    override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation.Write[G] = new Write[G]:
      override def printer[A](name: String, encode: A => String): G[A] = fK(self.printer(name, encode))

      override def string: G[String] = fK(self.string)

  object Write:
    inline def apply[F[_]](using self: StringOperation.Write[F]): StringOperation.Write[F] = self

    given InvariantK[StringOperation.Write] with
      extension [F[_]](self: StringOperation.Write[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation.Write[G] =
          self.imapK(fK)(gK)

  inline def apply[F[_]](using self: StringOperation[F]): StringOperation[F] = self

  given InvariantK[StringOperation] with
    extension [F[_]](self: StringOperation[F])
      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): StringOperation[G] =
        self.imapK(fK)(gK)
