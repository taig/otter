package io.taig.otter

import cats.syntax.all.*

abstract class RecordInvariant[Self[_], Field[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Record[Field, A]): Self[A]
  def extract[A](codec: Self[A]): Record[Field, A]

  final val empty: Self[Unit] = lift(Record.Empty(metadata = Metadata.Empty))
  final def one[A](field: => Field[A]): Self[A] = lift(
    Record.Root(field = Reference.later(field), metadata = Metadata.Empty)
  )

  extension [A](self: Self[A])
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
    final override def metadata: Metadata = extract(self).metadata
    final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
    final def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))
    final def :*[B](field: Field[B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(one(field)).imap(merge.apply)(merge.unapply)

  extension [A](self: Field[A])
    final def *:[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      one(self).zip(codec).imap(merge.apply)(merge.unapply)
