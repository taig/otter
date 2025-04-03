package io.taig.otter

abstract class TupleInvariant[Self[_], -Value[_]] extends CodecInvariant[Self]:
  def empty: Self[Unit]
  def one[A](codec: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def zip[B](codec: Self[B]): Self[(A, B)]
    final def :*[B](codec: Value[B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(one(codec)).imap(merge.apply)(merge.unapply)

  extension [A](self: Value[A])
    final def *:[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      one(self).zip(codec).imap(merge.apply)(merge.unapply)

object TupleInvariant:
  def apply[Self[_], Value[_]](
      lift: [A] => Tuple[Value, A] => Self[A],
      extract: [A] => Self[A] => Tuple[Value, A]
  ): TupleInvariant[Self, Value] = new TupleInvariant[Self, Value]:
    override val empty: Self[Unit] = lift(Tuple.Empty(metadata = Metadata.Empty))
    override def one[A](codec: => Value[A]): Self[A] =
      lift(Tuple.Root(codec = Reference.later(codec), metadata = Metadata.Empty))

    extension [A](self: Self[A])
      override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))
