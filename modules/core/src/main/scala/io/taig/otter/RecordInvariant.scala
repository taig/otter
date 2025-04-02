package io.taig.otter

import cats.syntax.all.*

abstract class RecordInvariant[Self[_], Field[_]] extends CodecInvariant[Self]:
  def empty: Self[Unit]
  def one[A](field: Field[A]): Self[A]

  extension [A](self: Self[A])
    def zip[B](codec: Self[B]): Self[(A, B)]

    final def :*[B](field: Field[B])(using merge: Merge[A, B]): Self[merge.Out] =
      imap(zip(one(field)))(merge.apply)(merge.unapply)

  extension [A](self: Field[A])
    final def *:[B](record: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      imap(one(self).zip(record))(merge.apply)(merge.unapply)
