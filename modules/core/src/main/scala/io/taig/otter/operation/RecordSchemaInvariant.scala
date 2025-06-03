package io.taig.otter.operation

import io.taig.otter.Merge

trait RecordSchemaInvariant[Self[_], -Field[_]] extends SchemaInvariant[Self]:
  self =>

  def lift[A](field: => Field[A]): Self[A]

  extension [A](self: Self[A])
    def zip[B](schema: Self[B]): Self[(A, B)]
    final def merge[B](schema: Self[B])(using m: Merge[A, B]): Self[m.Out] = zip(schema).merged
    final def :*[B](field: => Field[B])(using m: Merge[A, B]): Self[m.Out] = merge(lift(field))

  extension [A](self: Field[A])
    final def *:[B](schema: => Self[B])(using m: Merge[B, A]): Self[m.Out] = schema.merge(lift(self))

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): RecordSchemaInvariant[T, Field] =
    new RecordSchemaInvariant[T, Field]:
      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A]) override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

object RecordSchemaInvariant:
  inline def apply[Self[_], Field[_]](using
      self: RecordSchemaInvariant[Self, Field]
  ): RecordSchemaInvariant[Self, Field] = self
