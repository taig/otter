package io.taig.otter.schema

import cats.arrow.FunctionK
import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Shape
import io.taig.otter.Reference
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Field[+S[_], +T[_], A] extends Schema[T, A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  override def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]

  final override def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, A]

  final def optional: Field[S, T, Option[A]] = Field.Optional(self = this)

object Field:
  final private[otter] case class Modify[+S[_], +T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[+S[_], +T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Field[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, B] = copy(value = value.mapK(fK))

  final private[otter] case class Optional[+S[_], +T[_], A](self: Field[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, Option[A]] = copy(self = self.mapK(fK))

  given [Key[_], Value[_]]: Shape.Field[Field[Key, Value, *], Key, Value] =
    new Shape.Field[Field[Key, Value, *], Key, Value]:

      override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] =
        Root(
          key = Reference.Constant(self = Reference.later(key), value = name),
          value = Reference.later(value),
          metadata = Metadata.Empty
        )

      extension [A](fa: Field[Key, Value, A])
        override def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] = fa.imap(f)(g)
        override def optional: Field[Key, Value, Option[A]] = fa.optional
        override def metadata: Metadata = fa.metadata
        override def modifyMetadata(f: Metadata => Metadata): Field[Key, Value, A] = fa.modifyMetadata(f)
