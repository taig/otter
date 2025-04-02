package io.taig.otter

import cats.Invariant

sealed abstract class Optional[+S[_], A] extends Codec[S, A]:
  def codec: Reference[S, ?]
  override def modifyMetadata(f: Metadata => Metadata): Optional[S, A]
  final override def imap[B](f: A => B)(g: B => A): Optional[S, B] = Optional.Modify(self = this, f, g)

object Optional:
  final private[otter] case class Modify[S[_], A, B](self: Optional[S, A], f: A => B, g: B => A) extends Optional[S, B]:
    export self.{codec, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[S[_], A](codec: Reference[S, A], value: A, metadata: Metadata)
      extends Optional[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, A] = copy(metadata = f(metadata))

  // final private[otter] case class Null(metadata: Metadata) extends Optional[Nothing, Unit]:
  //   override def modifyMetadata(f: Metadata => Metadata): Optional[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Nullable[S[_], A](codec: Reference[S, A], metadata: Metadata)
      extends Optional[S, Option[A]]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, Option[A]] = copy(metadata = f(metadata))

  // final private[otter] case class Void[F , A](metadata: Metadata) extends Optional[F, Unit]:
  //   override def modifyMetadata(f: Metadata => Metadata): Optional[F, Unit] = copy(metadata = f(metadata))

  given [S[_]]: Invariant[Optional[S, *]] with
    override def imap[A, B](fa: Optional[S, A])(f: A => B)(g: B => A): Optional[S, B] = fa.imap(f)(g)
