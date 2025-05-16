package io.taig.otter

import io.taig.otter.Metadata
import io.taig.otter.schema.BranchSchema

sealed abstract class Branch[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, A]

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] =
      copy(value = value.mapK[T1, U](fK))

  given [Key[_], Value[_]]: BranchSchema[Branch[Key, Value, *], Key, Value] with
    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Branch[Key, Value, B] = Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )
    override def key[A](self: Branch[Key, Value, A]): Reference.Constant[Key, ?] = self.key
    override def value[A](self: Branch[Key, Value, A]): Reference[Value, ?] = self.value
    override def metadata[A](self: Branch[Key, Value, A]): Metadata = self.metadata
    override def modifyMetadata[A](self: Branch[Key, Value, A])(f: Metadata => Metadata): Branch[Key, Value, A] =
      self.modifyMetadata(f)
    override def imap[A, B](fa: Branch[Key, Value, A])(f: A => B)(g: B => A): Branch[Key, Value, B] =
      fa.imap(f)(g)
