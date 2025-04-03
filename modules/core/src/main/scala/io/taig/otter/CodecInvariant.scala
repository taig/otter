package io.taig.otter

import cats.Invariant

abstract class CodecInvariant[Self[_]]:
  self =>

  extension [A](self: Self[A])
    def imap[B](f: A => B)(g: B => A): Self[B]
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]

  final def invariant: Invariant[Self] = new Invariant[Self]:
    override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = self.imap(fa)(f)(g)

object CodecInvariant:
  trait Nullable[Self[_], Optional[_]] extends CodecInvariant[Self]:
    given optional: OptionalInvariant[Optional, Self]

    extension [A](self: Self[A])
      final def nullable: Optional[Option[A]] = optional.nullable(codec = self)
      final def nullable(default: A): Optional[A] = optional.nullable(codec = self, default)

  trait Tupleable[Self[_], Tuple[_]]:
    given tuple: TupleInvariant[Tuple, Self]

    extension [A](self: Self[A])
      final def :*[B](codec: Self[B])(using merge: Merge[A, B]): Tuple[merge.Out] =
        self.toTuple.zip(codec.toTuple).imap(merge.apply)(merge.unapply)
      final def *:[B](codec: Self[B])(using merge: Merge[B, A]): Tuple[merge.Out] =
        codec.toTuple.zip(self.toTuple).imap(merge.apply)(merge.unapply)
      final def toTuple: Tuple[A] = tuple.one(self)
