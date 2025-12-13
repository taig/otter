package io.taig.otter

import cats.data.Chain
import io.taig.validation.Validation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import cats.Functor

trait Primitive[F[_]]:
  def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive[G] = new Primitive[G] {}

object Primitive:
  trait Read[F[_]] extends Primitive[F]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Read[G] = new Read[G] {}

  object Read:
    inline def apply[F[_]](using self: Primitive.Read[F]): Primitive.Read[F] = self

    given FunctorK[Primitive.Read] with
      extension [G[_]](fa: Primitive.Read[G])
        override def mapK[H[_]](fK: [A] => G[A] => H[A]): Primitive.Read[H] = fa.mapK(fK)

  trait Write[F[_]] extends Primitive[F]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Write[G] = new Write[G] {}

  object Write:
    inline def apply[F[_]](using self: Primitive.Write[F]): Primitive.Write[F] = self

    given FunctorK[Primitive.Write] with
      extension [G[_]](fa: Primitive.Write[G])
        override def mapK[H[_]](fK: [A] => G[A] => H[A]): Primitive.Write[H] = fa.mapK(fK)

  trait Boolean[F[_]] extends Primitive[F]:
    self =>

    def boolean: F[SBoolean]

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Boolean[G] = new Boolean[G]:
      override def boolean: G[SBoolean] = fK(self.boolean)

  object Boolean:
    trait Read[F[_]] extends Primitive.Read[F], Primitive.Boolean[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Boolean.Read[G] = new Read[G]:
        override def boolean: G[SBoolean] = fK(self.boolean)

    object Read:
      inline def apply[F[_]](using self: Primitive.Boolean.Read[F]): Primitive.Boolean.Read[F] = self

      given FunctorK[Primitive.Boolean.Read] with
        extension [G[_]](fa: Primitive.Boolean.Read[G])
          override def mapK[H[_]](fK: [A] => G[A] => H[A]): Primitive.Boolean.Read[H] = fa.mapK(fK)

    trait Write[F[_]] extends Primitive.Read[F], Primitive.Boolean[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Boolean.Write[G] = new Write[G]:
        override def boolean: G[SBoolean] = fK(self.boolean)

    object Write:
      inline def apply[F[_]](using self: Primitive.Boolean.Write[F]): Primitive.Boolean.Write[F] = self

      given FunctorK[Primitive.Boolean.Write] with
        extension [G[_]](fa: Primitive.Boolean.Write[G])
          override def mapK[H[_]](fK: [A] => G[A] => H[A]): Primitive.Boolean.Write[H] = fa.mapK(fK)

    inline def apply[F[_]](using self: Primitive.Boolean[F]): Primitive.Boolean[F] = self

    given FunctorK[Primitive.Boolean] with
      extension [G[_]](fa: Primitive.Boolean[G])
        override def mapK[H[_]](fK: [A] => G[A] => H[A]): Primitive.Boolean[H] =
          fa.mapK(fK)

  trait Number[F[_]] extends Primitive[F]:
    self =>

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal]

    def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger]

    def constraints[A](self: F[A]): Chain[Constraint.Primitive.Number]

    def double(validation: Validation[Constraint.Primitive.Number, SDouble]): F[SDouble]

    def float(validation: Validation[Constraint.Primitive.Number, SFloat]): F[SFloat]

    def int(validation: Validation[Constraint.Primitive.Number, SInt]): F[SInt]

    def long(validation: Validation[Constraint.Primitive.Number, SLong]): F[SLong]

    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Number[G] = new Number[G]:
      override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
        fK(self.bigDecimal(validation))

      override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
        fK(self.bigInteger(validation))

      override def constraints[A](ga: G[A]): Chain[Constraint.Primitive.Number] = self.constraints(gK(ga))

      override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): G[SDouble] =
        fK(self.double(validation))

      override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): G[SFloat] =
        fK(self.float(validation))

      override def int(validation: Validation[Constraint.Primitive.Number, SInt]): G[SInt] =
        fK(self.int(validation))

      override def long(validation: Validation[Constraint.Primitive.Number, SLong]): G[SLong] =
        fK(self.long(validation))

  object Number:
    trait Read[F[_]] extends Primitive.Read[F], Primitive.Number[F]:
      self =>

      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Number.Read[G] =
        new Read[G]:
          override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
            fK(self.bigDecimal(validation))

          override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
            fK(self.bigInteger(validation))

          override def constraints[A](ga: G[A]): Chain[Constraint.Primitive.Number] = self.constraints(gK(ga))

          override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): G[SDouble] =
            fK(self.double(validation))

          override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): G[SFloat] =
            fK(self.float(validation))

          override def int(validation: Validation[Constraint.Primitive.Number, SInt]): G[SInt] =
            fK(self.int(validation))

          override def long(validation: Validation[Constraint.Primitive.Number, SLong]): G[SLong] =
            fK(self.long(validation))

    object Read:
      inline def apply[F[_]](using self: Primitive.Number.Read[F]): Primitive.Number.Read[F] = self

      given InvariantK[Primitive.Number.Read] with
        extension [G[_]](self: Primitive.Number.Read[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Number.Read[H] =
            self.imapK(fK)(gK)

    trait Write[F[_]] extends Primitive.Read[F], Primitive.Number[F]:
      self =>

      def bigDecimal: F[JBigDecimal]

      final override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal] =
        bigDecimal

      def bigInteger: F[JBigInteger]

      final override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger] =
        bigInteger

      final override def constraints[A](self: F[A]): Chain[Constraint.Primitive.Number] = Chain.empty

      def double: F[SDouble]

      final override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): F[SDouble] = double

      def float: F[SFloat]

      final override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): F[SFloat] = float

      def int: F[SInt]

      final override def int(validation: Validation[Constraint.Primitive.Number, SInt]): F[SInt] = int

      def long: F[SLong]

      final override def long(validation: Validation[Constraint.Primitive.Number, SLong]): F[SLong] = long

      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Number.Write[G] =
        new Write[G]:
          override def bigDecimal: G[JBigDecimal] = fK(self.bigDecimal)

          override def bigInteger: G[JBigInteger] = fK(self.bigInteger)

          override def double: G[SDouble] = fK(self.double)

          override def float: G[SFloat] = fK(self.float)

          override def int: G[SInt] = fK(self.int)

          override def long: G[SLong] = fK(self.long)

    object Write:
      inline def apply[F[_]](using self: Primitive.Number.Write[F]): Primitive.Number.Write[F] = self

      given InvariantK[Primitive.Number.Write] with
        extension [G[_]](self: Primitive.Number.Write[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Number.Write[H] =
            self.imapK(fK)(gK)

    inline def apply[F[_]](using self: Primitive.Number[F]): Primitive.Number[F] = self

    given InvariantK[Primitive.Number] with
      extension [G[_]](self: Primitive.Number[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Number[H] =
          self.imapK(fK)(gK)

  trait Text[F[_]] extends Primitive[F]:
    self =>

    def codec[A](name: JString, parse: JString => Either[JString, A], print: A => JString): F[A]

    def constraints[A](self: F[A]): Chain[Constraint.Primitive.Text]

    def string(validation: Validation[Constraint.Primitive.Text, JString]): F[JString]

    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Text[G] = new Primitive.Text[G]:
      override def codec[A](name: JString, parse: JString => Either[JString, A], print: A => JString): G[A] =
        fK(self.codec(name, parse, print))

      override def constraints[A](ga: G[A]): Chain[Constraint.Primitive.Text] = self.constraints(gK(ga))

      override def string(validation: Validation[Constraint.Primitive.Text, JString]): G[JString] =
        fK(self.string(validation))

  object Text:
    trait Read[F[_]] extends Primitive.Read[F], Primitive.Text[F]:
      self =>

      final override def codec[A](
          name: JString,
          parse: JString => Either[JString, A],
          print: A => JString
      ): F[A] = parser(name, parse)

      def parser[A](name: JString, parse: JString => Either[JString, A]): F[A]

      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Text.Read[G] =
        new Read[G]:
          override def constraints[A](ga: G[A]): Chain[Constraint.Primitive.Text] = self.constraints(gK(ga))

          override def parser[A](name: JString, parse: JString => Either[JString, A]): G[A] =
            fK(self.parser(name, parse))

          override def string(validation: Validation[Constraint.Primitive.Text, JString]): G[JString] =
            fK(self.string(validation))

    object Read:
      inline def apply[F[_]](using self: Primitive.Text.Read[F]): Primitive.Text.Read[F] = self

      given InvariantK[Primitive.Text.Read] with
        extension [G[_]](self: Primitive.Text.Read[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Text.Read[H] =
            self.imapK(fK)(gK)

    trait Write[F[_]] extends Primitive.Read[F], Primitive.Text[F]:
      self =>

      final override def constraints[A](self: F[A]): Chain[Constraint.Primitive.Text] = Chain.empty

      final override def codec[A](name: JString, parse: JString => Either[JString, A], print: A => JString): F[A] =
        printer(name, print)

      def printer[A](name: JString, print: A => JString): F[A]

      def string: F[JString]

      final override def string(validation: Validation[Constraint.Primitive.Text, JString]): F[JString] = string

      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Primitive.Text.Write[G] =
        new Write[G]:
          override def printer[A](name: JString, print: A => JString): G[A] = fK(self.printer(name, print))

          override def string: G[JString] = fK(self.string)

    object Write:
      inline def apply[F[_]](using self: Primitive.Text.Write[F]): Primitive.Text.Write[F] = self

      given InvariantK[Primitive.Text.Write] with
        extension [G[_]](self: Primitive.Text.Write[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Text.Write[H] =
            self.imapK(fK)(gK)

    inline def apply[F[_]](using self: Primitive.Text[F]): Primitive.Text[F] = self

    given InvariantK[Primitive.Text] with
      extension [G[_]](self: Primitive.Text[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive.Text[H] =
          self.imapK(fK)(gK)

  inline def apply[F[_]](using self: Primitive[F]): Primitive[F] = self

  given InvariantK[Primitive] with
    extension [G[_]](self: Primitive[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Primitive[H] = self.mapK(fK)
