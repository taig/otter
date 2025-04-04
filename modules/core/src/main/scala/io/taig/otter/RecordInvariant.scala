package io.taig.otter

trait RecordInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def empty: Self[Unit]

  def field[A](name: String, codec: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def isOptional: Boolean
    def optional: Self[Option[A]]
    def zip[B](codec: Self[B]): Self[(A, B)]
    final def :*[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(codec).imap(merge.apply)(merge.unapply)
    final def *:[B](codec: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      self.zip(codec).imap(merge.apply)(merge.unapply)

object RecordInvariant:
  def apply[Self[_], Value[_]](
      lift: [A] => (codec: Record[Value, A]) => Self[A],
      extract: [A] => (codec: Self[A]) => Record[Value, A]
  ): RecordInvariant[Self, Value] = new RecordInvariant[Self, Value]:
    override val empty: Self[Unit] = lift(Record.Empty(metadata = Metadata.Empty))
    override def field[A](name: String, codec: => Value[A]): Self[A] =
      lift(Record.Field(name, codec = Reference.later(codec), metadata = Metadata.Empty))

    extension [A](self: Self[A])
      override def isOptional: Boolean = extract(self).isOptional
      override def optional: Self[Option[A]] = lift(extract(self).optional)
      override def zip[B](codec: Self[B]): Self[(A, B)] =
        lift(extract(self).zip(extract(codec)))

    extension [A](self: Self[A])
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] =
        lift(extract(self).modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
