package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Reference

trait FieldSchema[Self[_], Key[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  extension [A](self: Self[A])
    def key: Reference.Constant[Key, ?]
    def value: Reference[Value, ?]

    def isOptional: Boolean

    def nullish: Boolean
    def modifyNullish(f: Boolean => Boolean): Self[A]
    final def nullish(value: Boolean): Self[A] = modifyNullish(_ => value)

    def optional: Self[Option[A]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): FieldSchema[T, Key, Value] = new FieldSchema[T, Key, Value]:

    override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] =
      fK(self.apply(name, key, value))

    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def key: Reference.Constant[Key, ?] = self.key(gK(ta))
      override def value: Reference[Value, ?] = self.value(gK(ta))
      override def isOptional: Boolean = self.isOptional(gK(ta))
      override def nullish: Boolean = self.nullish(gK(ta))
      override def modifyNullish(f: Boolean => Boolean): T[A] = fK(self.modifyNullish(gK(ta))(f))
      override def optional: T[Option[A]] = fK(self.optional(gK(ta)))
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object FieldSchema:
  inline def apply[Self[_], Key[_], Value[_]](using
      self: FieldSchema[Self, Key, Value]
  ): FieldSchema[Self, Key, Value] = self
