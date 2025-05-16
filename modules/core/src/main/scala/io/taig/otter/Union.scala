package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Metadata

sealed abstract class Union[+S[_], A] extends Product with Serializable:
  def schemas: NonEmptyChain[Reference[S, ?]]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Union[S, A]

  final def imap[B](f: A => B)(g: B => A): Union[S, B] = Union.Modify(self = this, f, g)

  final def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union[S1, Either[A, B]] =
    Union.OrElse(left = this, right = codec, metadata = Metadata.Empty)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A]

object Union:
  final private[otter] case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.{metadata, schemas}
    override def modifyMetadata(f: Metadata => Metadata): Union[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class OrElse[S[_], A, B](
      left: Union[S, A],
      right: Union[S, B],
      metadata: Metadata
  ) extends Union[S, Either[A, B]]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = left.schemas ++ right.schemas
    override def modifyMetadata(f: Metadata => Metadata): Union[S, Either[A, B]] =
      copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, Either[A, B]] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](codec: Reference[S, A], metadata: Metadata) extends Union[S, A]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(codec)
    override def modifyMetadata(f: Metadata => Metadata): Union[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A] = copy(codec = codec.mapK[S1, T](fK))

  // given [Value[_]]: UnionSchema[Union[Value, *], Value] with
  //   override def lift[A](schema: => Value[A]): Union[Value, A] = Union.Root(
  //     codec = Reference.later(schema),
  //     metadata = Metadata.Empty
  //   )

  //   extension [A](self: Union[Value, A])
  //     override def metadata: Metadata = self.metadata
  //     override def modifyMetadata(f: Metadata => Metadata): Union[Value, A] = self.modifyMetadata(f)
  //     override def imap[B](f: A => B)(g: B => A): Union[Value, B] = self.imap(f)(g)
  //     override def orElse[B](codec: Union[Value, B]): Union[Value, Either[A, B]] = self.orElse(codec)
