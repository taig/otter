package io.taig.otter.schema

import io.taig.otter.Metadata

trait DictionarySchema[Self[_], -Key[_], -Value[_]] extends Schema[Self]:
  self =>

  def dictionary[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int],
      maximum: Option[Int]
  ): Self[List[(A, B)]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): DictionarySchema[T, Key, Value] = new DictionarySchema[T, Key, Value]:
    override def dictionary[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): T[List[(A, B)]] = fK(self.dictionary(key, value, minimum, maximum))

    override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
    override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object DictionarySchema:
  inline def apply[Self[_], Key[_], Value[_]](using
      self: DictionarySchema[Self, Key, Value]
  ): DictionarySchema[Self, Key, Value] = self
