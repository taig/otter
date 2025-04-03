package io.taig.otter

abstract class TupleInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Tuple[Value, A]): Self[A]
  def extract[A](codec: Self[A]): Tuple[Value, A]

  final val empty: Self[Unit] = lift(Tuple.Empty(metadata = Metadata.Empty))
  final def one[A](codec: => Value[A]): Self[A] = lift(
    Tuple.Root(codec = Reference.later(codec), metadata = Metadata.Empty)
  )

  extension [A](self: Self[A])
    final override def metadata: Metadata = extract(self).metadata
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
    final def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))
    final def :*[B](codec: Value[B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(one(codec)).imap(merge.apply)(merge.unapply)

  extension [A](self: Value[A])
    final def *:[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      one(self).zip(codec).imap(merge.apply)(merge.unapply)
