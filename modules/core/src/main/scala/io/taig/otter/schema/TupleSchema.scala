package io.taig.otter.schema

trait TupleSchema[Self[_], -Value[_]] extends Schema[Self]:
  self =>

  def empty: Self[Unit]
  def lift[A](schema: => Value[A]): Self[A]

  extension [A](self: Self[A]) def zip[B](schema: Self[B]): Self[(A, B)]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): TupleSchema[T, Value] =
    new TupleSchema[T, Value]:
      override def empty: T[Unit] = fK(self.empty)
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A]) override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

object TupleSchema:
  inline def apply[Self[_], Field[_]](using self: TupleSchema[Self, Field]): TupleSchema[Self, Field] = self
