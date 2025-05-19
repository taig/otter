package io.taig.otter

import io.taig.otter.Metadata
import io.taig.otter.schema.FieldSchema

sealed abstract class Field[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def isOptional: Boolean

  def nullish: Boolean
  def nullish(f: Boolean => Boolean): Field[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, A]

  final def optional: Field[S, T, Option[A]] = Field.Optional(self = this)

object Field:
  final private[otter] case class Modify[+S[_], +T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{isOptional, key, value}
    override def nullish: Boolean = self.nullish
    override def nullish(f: Boolean => Boolean): Field[S, T, B] = copy(self = self.nullish(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] = copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Optional[S[_], T[_], A](self: Field[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, value}
    override def isOptional: Boolean = true
    override def nullish: Boolean = self.nullish
    override def nullish(f: Boolean => Boolean): Field[S, T, Option[A]] = copy(self = self.nullish(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, Option[A]] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[+S[_], +T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      nullish: Boolean
  ) extends Field[S, T, B]:
    override def isOptional: Boolean = false
    override def nullish(f: Boolean => Boolean): Field[S, T, B] = copy(nullish = f(nullish))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] =
      copy(value = value.mapK[T1, U](fK))

  given [Key[_], Value[_], Record[_]]: FieldSchema[Field[Key, Value, *], Key, Value, Record] with
    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] = Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      nullish = true
    )

    override def imap[A, B](fa: Field[Key, Value, A])(f: A => B)(g: B => A): Field[Key, Value, B] = fa.imap(f)(g)

    extension [A](self: Field[Key, Value, A])
      override def key: Reference.Constant[Key, ?] = self.key
      override def value: Reference[Value, ?] = self.value
      override def isOptional: Boolean = self.isOptional
      override def nullish: Boolean = self.nullish
      override def nullish(f: Boolean => Boolean): Field[Key, Value, A] = self.nullish(f)
      override def optional: Field[Key, Value, Option[A]] = self.optional
      override def toRecord: Record[A] = ???
