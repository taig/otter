package io.taig.otter
import cats.implicits.*
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.DictionarySchemaInvariant

final case class Dictionary[+S[_], +T[_], A](value: Dictionary.Value[S, T, A], metadata: Metadata)

object Dictionary:
  sealed abstract class Value[+S[_], +T[_], A] extends Product, Serializable:
    def key: Reference[S, ?]
    def value: Reference[T, ?]

    def constraints: Vector[Constraint.Object]

    def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, A]

    def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Value[U, T, A]
    final def imap[B](f: A => B)(g: B => A): Value[S, T, B] = Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Root[S[_], T[_], A, B](
        key: Reference[S, A],
        value: Reference[T, B],
        minimum: Option[Int],
        maximum: Option[Int]
    ) extends Value[S, T, List[(A, B)]]:
      override def constraints: Vector[Constraint.Object] = Vector(
        minimum.map(Constraint.Object.Minimum.apply),
        maximum.map(Constraint.Object.Maximum.apply)
      ).flatten
      override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, List[(A, B)]] =
        copy(value = value.mapK[T1, U](fK))
      override def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Value[U, T, List[(A, B)]] =
        copy(key = key.mapK[S1, U](fK))

    final private[otter] case class Modify[S[_], T[_], A, B](self: Value[S, T, A], f: A => B, g: B => A)
        extends Value[S, T, B]:
      export self.{constraints, key, value}
      override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Value[S, U, B] =
        copy(self = self.mapK[T1, U](fK))
      override def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Value[U, T, B] =
        copy(self = self.leftMapK[S1, U](fK))

  given [Key[_], Value[_]]: DictionarySchemaInvariant[Dictionary[Key, Value, *], Key, Value] with
    override def apply[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): Dictionary[Key, Value, List[(A, B)]] = Dictionary(
      value = Value.Root(
        key = Reference.later(key),
        value = Reference.later(value),
        minimum = minimum,
        maximum = maximum
      ),
      metadata = Metadata.Empty
    )

    override def imap[A, B](fa: Dictionary[Key, Value, A])(f: A => B)(g: B => A): Dictionary[Key, Value, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Dictionary[Key, Value, A]] = new Enriched[Dictionary[Key, Value, A]]:
      override def metadata(a: Dictionary[Key, Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Dictionary[Key, Value, A])(f: Metadata => Metadata): Dictionary[Key, Value, A] =
        a.copy(metadata = f(a.metadata))
