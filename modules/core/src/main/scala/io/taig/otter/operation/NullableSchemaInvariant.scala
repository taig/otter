package io.taig.otter.operation

trait NullableSchemaInvariant[Self[_], -Value[_]] extends SchemaInvariant[Self]:
  self =>

  def apply[A](schema: => Value[A]): Self[Option[A]]
  def apply[A](schema: => Value[A], default: A): Self[A]
  def void: Self[Unit]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): NullableSchemaInvariant[T, Value] =
    new NullableSchemaInvariant[T, Value]:
      override def apply[A](schema: => Value[A]): T[Option[A]] = fK(self.apply(schema))
      override def apply[A](schema: => Value[A], default: A): T[A] = fK(self.apply(schema, default))
      override def void: T[Unit] = fK(self.void)
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object NullableSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      self: NullableSchemaInvariant[Self, Value]
  ): NullableSchemaInvariant[Self, Value] = self
