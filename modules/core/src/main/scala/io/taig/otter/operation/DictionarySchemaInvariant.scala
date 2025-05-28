package io.taig.otter.operation

import io.taig.otter.Metadata

trait DictionarySchemaInvariant[Self[_], -Key[_], -Value[_]] extends SchemaInvariant[Self]:
  self =>

  def apply[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int],
      maximum: Option[Int]
  ): Self[List[(A, B)]]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): DictionarySchemaInvariant[T, Key, Value] = new DictionarySchemaInvariant[T, Key, Value]:
    override def apply[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): T[List[(A, B)]] = fK(self(key, value, minimum, maximum))

    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def metadata: Metadata = self.metadata(gK(ta))
      override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

object DictionarySchemaInvariant:
  inline def apply[Self[_], Key[_], Value[_]](using
      self: DictionarySchemaInvariant[Self, Key, Value]
  ): DictionarySchemaInvariant[Self, Key, Value] = self
