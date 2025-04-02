package io.taig.otter

abstract class FieldInvariant[Self[_], Record[_], Key[_], Value[_]](using record: RecordInvariant[Record, Self])
    extends CodecInvariant[Self]:
  def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  extension [A](self: Self[A])
    final def toRecord: Record[A] = record.one(field = self)
    final def :*[B](field: Self[B])(using merge: Merge[A, B]): Record[merge.Out] =
      record.one(field = self) :* field
    final def *:[B](field: Self[B])(using merge: Merge[A, B]): Record[merge.Out] =
      record.one(field = self)
      ???
