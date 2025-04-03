package io.taig.otter

abstract class FieldInvariant[Key[_], Value[_], Record[_]](using record: RecordInvariant[Record, Key, Value])
    extends CodecInvariant[Field[Key, Value, *]]:
  final def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Field.Required[Key, Value, B] =
    Field.Required.Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )

  extension [A](self: Field[Key, Value, A])
    final override def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] = self.imap(f)(g)
    final override def metadata: Metadata = self.metadata
    final def toRecord: Record[A] = record.one(field = self)
    final def :*[B](field: Field[Key, Value, B])(using merge: Merge[A, B]): Record[merge.Out] =
      record.one(field = self) :* field
    final def *:[B](field: Field[Key, Value, B])(using merge: Merge[B, A]): Record[merge.Out] =
      field *: record.one(field = self)
