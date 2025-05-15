package io.taig.otter

import io.taig.otter.Metadata
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.schema.Schema
import io.taig.otter.schema.FieldSchema

sealed abstract class Field[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, A]

  final def optional: Field[S, T, Option[A]] = Field.Optional(self = this)
  final def nullish: Field[S, T, A] = Field.Nullish(self = this)

object Field:
  final private[otter] case class Modify[+S[_], +T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] = copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Nullish[S[_], T[_], A](self: Field[S, T, A]) extends Field[S, T, A]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, A] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, A] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Optional[S[_], T[_], A](self: Field[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, Option[A]] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[+S[_], +T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Field[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] =
      copy(value = value.mapK[T1, U](fK))

  given [Key[_], Value[_]]: FieldSchema[Field[Key, Value, *], Key, Value] with
    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] = Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )

    extension [A](self: Field[Key, Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[Key, Value, A] = self.modifyMetadata(f)
      override def key: Reference.Constant[Key, ?] = self.key
      override def value: Reference[Value, ?] = self.value
      override def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] = self.imap(f)(g)
      override def nullish: Field[Key, Value, A] = self.nullish
      override def optional: Field[Key, Value, Option[A]] = self.optional
