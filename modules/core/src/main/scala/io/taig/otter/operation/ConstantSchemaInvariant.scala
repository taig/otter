// package io.taig.otter.operation

// import cats.Eq

// trait ConstantSchemaInvariant[Self[_], -Value[_]] extends SchemaInvariant[Self]:
//   self =>

//   def apply[A: Eq](schema: => Value[A], value: A): Self[Unit]

//   override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): ConstantSchemaInvariant[T, Value] =
//     new ConstantSchemaInvariant[T, Value]:
//       override def apply[A](schema: => Value[A], value: A)(using Eq[A]): T[Unit] = fK(self.apply(schema, value))
//       override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
//       override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

// object ConstantSchemaInvariant:
//   inline def apply[Self[_], Value[_]](using
//       self: ConstantSchemaInvariant[Self, Value]
//   ): ConstantSchemaInvariant[Self, Value] = self
