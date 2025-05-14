package io.taig.otter.schema

import cats.Eq
import cats.syntax.all.*
import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Shape

sealed abstract class Constant[+S[_], A] extends Schema[S, A]:
  def metadata: Metadata
  def schema: Reference.Constant[S, ?]
  def modifyMetadata(f: Metadata => Metadata): Constant[S, A]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, A]
  final def imap[B](f: A => B)(g: B => A): Constant[S, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.{metadata, schema}
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A](
      schema: Reference.Constant[S, A],
      eq: Eq[A],
      metadata: Metadata
  ) extends Constant[S, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, Unit] = copy(schema = schema.mapK(fK))

  given [Value[_]]: Shape.Constant[Constant[Value, *], Value] with
    override def constant[A](schema: => Value[A], value: A)(using eq: Eq[A]): Constant[Value, Unit] = Root(
      schema = Reference.Constant(self = Reference.later(schema), value),
      eq,
      metadata = Metadata.Empty
    )

    extension [A](fa: Constant[Value, A])
      override def imap[B](f: A => B)(g: B => A): Constant[Value, B] = fa.imap(f)(g)
      override def modifyMetadata(f: Metadata => Metadata): Constant[Value, A] = fa.modifyMetadata(f)
      override def metadata: Metadata = fa.metadata
