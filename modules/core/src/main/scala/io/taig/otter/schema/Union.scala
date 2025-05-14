package io.taig.otter.schema

import cats.syntax.all.*
import cats.~>
import cats.data.NonEmptyChain
import io.taig.otter.Reference
import io.taig.otter.Metadata
import io.taig.otter.Shape

sealed abstract class Union[+S[_], A] extends Schema[S, A]:
  def schemas: NonEmptyChain[Reference[S, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Union[S, A]

  final override def imap[B](f: A => B)(g: B => A): Union[S, B] = Union.Modify(self = this, f, g)

  final def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union[S1, Either[A, B]] =
    Union.OrElse(left = this, right = codec, metadata = Metadata.Empty)

  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, A]

object Union:
  final private[otter] case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.{metadata, schemas}
    override def modifyMetadata(f: Metadata => Metadata): Union[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class OrElse[S[_], A, B](
      left: Union[S, A],
      right: Union[S, B],
      metadata: Metadata
  ) extends Union[S, Either[A, B]]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = left.schemas ++ right.schemas
    override def modifyMetadata(f: Metadata => Metadata): Union[S, Either[A, B]] =
      copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, Either[A, B]] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final private[otter] case class Root[S[_], A](codec: Reference[S, A], metadata: Metadata) extends Union[S, A]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(codec)
    override def modifyMetadata(f: Metadata => Metadata): Union[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, A] = copy(codec = codec.mapK(fK))

  given [Value[_]]: Shape.Union[Union[Value, *], Value] = new Shape.Union[Union[Value, *], Value]:
    override def one[A](schema: => Value[A]): Union[Value, A] = Union.Root(
      codec = Reference.later(schema),
      metadata = Metadata.Empty
    )
    extension [A](self: Union[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Union[Value, B] = self.imap(f)(g)
