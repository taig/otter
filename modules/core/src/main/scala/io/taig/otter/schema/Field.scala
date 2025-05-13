package io.taig.otter.schema

import cats.arrow.FunctionK
import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference

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
