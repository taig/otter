// package io.taig.otter.operation

// trait DictionarySchemaInvariant[Self[_], -Key[_], -Value[_]] extends SchemaInvariant[Self]:
//   self =>

//   def apply[A, B](
//       key: => Key[A],
//       value: => Value[B],
//       minimum: Option[Int],
//       maximum: Option[Int]
//   ): Self[List[(A, B)]]

//   override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
//       gK: [A] => T[A] => Self[A]
//   ): DictionarySchemaInvariant[T, Key, Value] = new DictionarySchemaInvariant[T, Key, Value]:
//     override def apply[A, B](
//         key: => Key[A],
//         value: => Value[B],
//         minimum: Option[Int],
//         maximum: Option[Int]
//     ): T[List[(A, B)]] = fK(self(key, value, minimum, maximum))

//     override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
//     override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

// object DictionarySchemaInvariant:
//   inline def apply[Self[_], Key[_], Value[_]](using
//       self: DictionarySchemaInvariant[Self, Key, Value]
//   ): DictionarySchemaInvariant[Self, Key, Value] = self
