package io.taig.otter

import cats.syntax.all.*

abstract class RecordInvariant[Self[_], Key[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Record[Key, Value, A]): Self[A]
  def extract[A](codec: Self[A]): Record[Key, Value, A]

  final val empty: Self[Unit] = lift(Record.Empty(metadata = Metadata.Empty))
  final def one[A](field: Field[Key, Value, A]): Self[A] = lift(Record.Root(field, metadata = Metadata.Empty))

  extension [A](self: Self[A])
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
    final override def metadata: Metadata = extract(self).metadata
    final def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))
    final def :*[B](field: Field[Key, Value, B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(one(field)).imap(merge.apply)(merge.unapply)

  extension [A](self: Field[Key, Value, A])
    final def *:[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      one(self).zip(codec).imap(merge.apply)(merge.unapply)
