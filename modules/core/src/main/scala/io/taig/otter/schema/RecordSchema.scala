package io.taig.otter.schema

trait RecordSchema[Self[_], -Field[_]] extends Schema[Self]:
  self =>

  def lift[A](field: => Field[A]): Self[A]

  extension [A](self: Self[A]) def zip[B](schema: Self[B]): Self[(A, B)]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): RecordSchema[T, Field] =
    new RecordSchema[T, Field]:
      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A]) override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

object RecordSchema:
  inline def apply[Self[_], Field[_]](using self: RecordSchema[Self, Field]): RecordSchema[Self, Field] = self
