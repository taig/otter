package io.taig.otter

trait FieldInvariant[Self[_], Record[_]] extends CodecInvariant[Self]:
  given record: RecordInvariant[Record, Self]

  extension [A](self: Self[A])
    final def :*[B](field: Self[B])(using merge: Merge[A, B]): Record[merge.Out] = toRecord :* field
    final def *:[B](field: Self[B])(using merge: Merge[B, A]): Record[merge.Out] = field *: toRecord

    final def toRecord: Record[A] = record.one(field = self)
