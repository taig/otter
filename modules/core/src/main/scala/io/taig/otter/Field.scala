package io.taig.otter

import io.taig.otter.operation.Enriched
import io.taig.otter.operation.FieldSchemaInvariant

final case class Field[+S[_], +T[_], A](value: Field.Value[S, T, A], metadata: Metadata)

object Field:
  sealed abstract class Value[+S[_], +T[_], A] extends Product, Serializable:
    def key: Reference.Constant[S, ?]
    def value: Reference[T, ?]

    def isOptional: Boolean

    def nullish: Boolean
    def nullish(f: Boolean => Boolean): Value[S, T, A]

    final def imap[B](f: A => B)(g: B => A): Value[S, T, B] = Value.Modify(self = this, f, g)

    def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, A]

    final def optional: Value[S, T, Option[A]] = Value.Optional(self = this)

  object Value:
    final private[otter] case class Modify[+S[_], +T[_], A, B](self: Value[S, T, A], f: A => B, g: B => A)
        extends Value[S, T, B]:
      export self.{isOptional, key, value}
      override def nullish: Boolean = self.nullish
      override def nullish(f: Boolean => Boolean): Value[S, T, B] = copy(self = self.nullish(f))
      override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, B] =
        copy(self = self.mapK[T1, U](fK))

    final private[otter] case class Optional[S[_], T[_], A](self: Value[S, T, A]) extends Value[S, T, Option[A]]:
      export self.{key, value}
      override def isOptional: Boolean = true
      override def nullish: Boolean = self.nullish
      override def nullish(f: Boolean => Boolean): Value[S, T, Option[A]] = copy(self = self.nullish(f))
      override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, Option[A]] =
        copy(self = self.mapK[T1, U](fK))

    final private[otter] case class Root[+S[_], +T[_], A, B](
        key: Reference.Constant[S, A],
        value: Reference[T, B],
        nullish: Boolean
    ) extends Value[S, T, B]:
      override def isOptional: Boolean = false
      override def nullish(f: Boolean => Boolean): Value[S, T, B] = copy(nullish = f(nullish))
      override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, B] =
        copy(value = value.mapK[T1, U](fK))

  given [Key[_], Value[_]]: FieldSchemaInvariant[Field[Key, Value, *], Key, Value] with
    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] = Field(
      value = Value.Root(
        key = Reference.Constant(self = Reference.later(key), value = name),
        value = Reference.later(value),
        nullish = true
      ),
      metadata = Metadata.Empty
    )

    override def imap[A, B](fa: Field[Key, Value, A])(f: A => B)(g: B => A): Field[Key, Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Field[Key, Value, A]] = new Enriched[Field[Key, Value, A]]:
      override def metadata(a: Field[Key, Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Field[Key, Value, A])(f: Metadata => Metadata): Field[Key, Value, A] =
        a.copy(metadata = f(a.metadata))

    extension [A](self: Field[Key, Value, A])
      override def key: Reference.Constant[Key, ?] = self.value.key
      override def value: Reference[Value, ?] = self.value.value
      override def isOptional: Boolean = self.value.isOptional
      override def nullish: Boolean = self.value.nullish
      override def nullish(f: Boolean => Boolean): Field[Key, Value, A] =
        self.copy(value = self.value.nullish(f))
      override def optional: Field[Key, Value, Option[A]] =
        self.copy(value = self.value.optional)
