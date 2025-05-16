package io.taig.otter
import cats.implicits.*
import io.taig.otter.Metadata

sealed abstract class Dictionary[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference[S, ?]
  def value: Reference[T, ?]

  def constraints: Vector[Constraint.Object]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, A]

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Dictionary[S, U, A]

  def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Dictionary[U, T, A]
  final def imap[B](f: A => B)(g: B => A): Dictionary[S, T, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference[S, A],
      value: Reference[T, B],
      minimum: Option[Int],
      maximum: Option[Int],
      metadata: Metadata
  ) extends Dictionary[S, T, List[(A, B)]]:
    override def constraints: Vector[Constraint.Object] = Vector(
      minimum.map(Constraint.Object.Minimum.apply),
      maximum.map(Constraint.Object.Maximum.apply)
    ).flatten
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, List[(A, B)]] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Dictionary[S, U, List[(A, B)]] =
      copy(value = value.mapK[T1, U](fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Dictionary[U, T, List[(A, B)]] =
      copy(key = key.mapK[S1, U](fK))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Dictionary[S, T, A], f: A => B, g: B => A)
      extends Dictionary[S, T, B]:
    export self.{constraints, key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Dictionary[S, U, B] =
      copy(self = self.mapK[T1, U](fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: [A] => S1[A] => U[A]): Dictionary[U, T, B] =
      copy(self = self.leftMapK[S1, U](fK))

  // given [Key[_], Value[_]]: DictionarySchema[Dictionary[Key, Value, *], Key, Value] with
  //   override def dictionary[A, B](
  //       key: => Key[A],
  //       value: => Value[B],
  //       minimum: Option[Int],
  //       maximum: Option[Int]
  //   ): Dictionary[Key, Value, List[(A, B)]] = Root(
  //     key = Reference.later(key),
  //     value = Reference.later(value),
  //     minimum,
  //     maximum,
  //     metadata = Metadata.Empty
  //   )

  //   extension [A](fa: Dictionary[Key, Value, A])
  //     override def imap[B](f: A => B)(g: B => A): Dictionary[Key, Value, B] = fa.imap(f)(g)
  //     override def modifyMetadata(f: Metadata => Metadata): Dictionary[Key, Value, A] = fa.modifyMetadata(f)
  //     override def metadata: Metadata = fa.metadata
