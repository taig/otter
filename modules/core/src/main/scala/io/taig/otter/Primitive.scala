package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.validation.Validation

sealed abstract class Primitive[A] extends Primitive.Read[A], Primitive.Write[A]:
  def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed trait Read[+A]:
    def map[B](f: A => B): Primitive.Read[B]

  object Read:
    given Functor[Primitive.Read]:
      override def map[A, B](fa: Primitive.Read[A])(f: A => B): Primitive.Read[B] = fa.map(f)

  sealed trait Write[-A]:
    def contramap[B](f: B => A): Primitive.Write[B]

  object Write:
    given Contravariant[Primitive.Write]:
      override def contramap[A, B](fa: Primitive.Write[A])(f: B => A): Primitive.Write[B] = fa.contramap(f)

  sealed abstract class Boolean[A] extends Primitive[A], Primitive.Boolean.Read[A], Primitive.Boolean.Write[A]:
    final override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Boolean.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Boolean.Read[A], f: A => B) extends Primitive.Boolean.Read[B]

      given Functor[Primitive.Boolean.Read]:
        override def map[A, B](fa: Primitive.Boolean.Read[A])(f: A => B): Primitive.Boolean.Read[B] =
          fa.map(f)

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Boolean.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Boolean.Write[A], f: B => A) extends Primitive.Boolean.Write[B]

      given Contravariant[Primitive.Boolean.Write]:
        override def contramap[A, B](fa: Primitive.Boolean.Write[A])(f: B => A): Primitive.Boolean.Write[B] =
          fa.contramap(f)

    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A) extends Primitive.Boolean[B]

    case object Root extends Primitive.Boolean[SBoolean]

    given Invariant[Primitive.Boolean]:
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        fa.imap(f)(g)

  sealed abstract class Number[A]:
    final def imap[B](f: A => B)(g: B => A): Primitive.Number[B] = Number.Modify(self = this, f, g)

  object Number:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Number.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Number.Read[A], f: A => B) extends Primitive.Number.Read[B]

      given Functor[Primitive.Number.Read]:
        override def map[A, B](fa: Primitive.Number.Read[A])(f: A => B): Primitive.Number.Read[B] = fa.map(f)

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Number.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Number.Write[A], f: B => A) extends Primitive.Number.Write[B]

      given Contravariant[Primitive.Number.Write]:
        override def contramap[A, B](fa: Primitive.Number.Write[A])(f: B => A): Primitive.Number.Write[B] =
          fa.contramap(f)

    final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
        extends Primitive.Number[JBigDecimal]

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends Primitive.Number[JBigInteger]

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends Primitive.Number[SDouble]

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat]) extends Primitive.Number[SFloat]

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt]) extends Primitive.Number[SInt]

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong]) extends Primitive.Number[SLong]

    final case class Modify[A, B](self: Primitive.Number[A], f: A => B, g: B => A) extends Primitive.Number[B]

    given Invariant[Primitive.Number]:
      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] = fa.imap(f)(g)

  sealed abstract class Text[A] extends Primitive[A], Primitive.Text.Read[A], Primitive.Text.Write[A]:
    final override def imap[B](f: A => B)(g: B => A): Primitive.Text[B] = Text.Modify(self = this, f, g)

  object Text:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Text.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Decoder[A](name: String, decode: String => Either[String, A]) extends Primitive.Text.Read[A]

      final case class Modify[A, B](self: Primitive.Text.Read[A], f: A => B) extends Primitive.Text.Read[B]

      given Functor[Primitive.Text.Read]:
        override def map[A, B](fa: Primitive.Text.Read[A])(f: A => B): Primitive.Text.Read[B] = fa.map(f)

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Text.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Encoder[A](name: String, encode: A => String) extends Primitive.Text.Write[A]

      final case class Modify[A, B](self: Primitive.Text.Write[A], f: B => A) extends Primitive.Text.Write[B]

      given Contravariant[Primitive.Text.Write]:
        override def contramap[A, B](fa: Primitive.Text.Write[A])(f: B => A): Primitive.Text.Write[B] =
          fa.contramap(f)

    final case class Modify[A, B](self: Primitive.Text[A], f: A => B, g: B => A) extends Primitive.Text[B]

    final case class Codec[A](name: String, decode: String => Either[String, A], encode: A => String)
        extends Primitive.Text[A]

    final case class Root(validation: Validation[Constraint.Primitive.Text, String]) extends Primitive.Text[String]

    given Invariant[Primitive.Text]:
      override def imap[A, B](fa: Primitive.Text[A])(f: A => B)(g: B => A): Primitive.Text[B] = fa.imap(f)(g)

  given Invariant[Primitive]:
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)
