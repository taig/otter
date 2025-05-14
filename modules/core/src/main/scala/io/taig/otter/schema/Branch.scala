package io.taig.otter.schema

import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Invariant
import io.taig.otter.Shape
import io.taig.otter.Reference.Constant

sealed abstract class Branch[+S[_], +T[_], A] extends Schema[T, A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, A]

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(value = value.mapK(fK))

  given [Key[_], Value[_]]: Shape.Branch[Branch[Key, Value, *], Key, Value] with
    override def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Branch[Key, Value, B] =
      Root(
        key = Reference.Constant(self = Reference.later(key), value = name),
        value = Reference.later(value),
        metadata = Metadata.Empty
      )

    extension [A](self: Branch[Key, Value, A])
      override def key: Constant[Key, ?] = self.key
      override def value: Reference[Value, ?] = self.value
      override def imap[B](f: A => B)(g: B => A): Branch[Key, Value, B] = self.imap(f)(g)
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[Key, Value, A] = self.modifyMetadata(f)
