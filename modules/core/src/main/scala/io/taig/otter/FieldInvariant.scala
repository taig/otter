package io.taig.otter

abstract class FieldInvariant[Self[_], Record[_]](using record: RecordInvariant[Record, Self])
    extends CodecInvariant[Self]:
  // final def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field.Required[Key, Value, B] =
  //   Field.Required.Root(
  //     key = Reference.Constant(self = Reference.later(key), value = name),
  //     value = Reference.later(value),
  //     metadata = Metadata.Empty
  //   )

  extension [A](self: Self[A])
    // final override def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] = self.imap(f)(g)
    //   final override def metadata: Metadata = self.metadata
    //   final override def modifyMetadata(f: Metadata => Metadata): Field[Key, Value, A] = self.modifyMetadata(f)

    final def :*[B](field: Self[B])(using merge: Merge[A, B]): Record[merge.Out] = toRecord :* field
    final def *:[B](field: Self[B])(using merge: Merge[B, A]): Record[merge.Out] = field *: toRecord

    final def toRecord: Record[A] = record.one(field = self)
