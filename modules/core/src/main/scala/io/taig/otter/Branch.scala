package io.taig.otter

import cats.syntax.all.*
import cats.Eval

final case class Branch[+F <: Data.Any, A](name: String, codec: Eval[Codec[F, A]], metadata: Metadata):
  def modifyMetadata(f: Metadata => Metadata): Branch[F, A] = copy(metadata = f(metadata))
  def imap[B](f: A => B)(g: B => A): Branch[F, B] = copy(codec = codec.map(_.imap(f)(g)))

  def :+[G <: Data.Any, B](branch: Branch[G, B]): Union[F | G, Either[A, B]] =
    toUnion :+ branch

  def toUnion: Union[F, A] = Union.Root(branch = this, metadata = Metadata.Empty)

object Branch:
  extension [F <: Data.Any, A <: Matchable](self: Branch[F, A])
    inline def |[G <: Data.Any, B <: Matchable](branch: Branch[G, B]): Union[F | G, A | B] =
      self.toUnion | branch
