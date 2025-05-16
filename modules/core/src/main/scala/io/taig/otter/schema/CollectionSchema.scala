package io.taig.otter.schema

import io.taig.otter.Metadata

trait CollectionSchema[Self[_], -Value[_]] extends Schema[Self]:
  self =>

  def linked[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], unique: Boolean): Self[List[A]]

  def indexed[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], unique: Boolean): Self[Vector[A]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): CollectionSchema[T, Value] = new CollectionSchema[T, Value]:
    override def linked[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): T[List[A]] = fK(self.linked(schema, minimum, maximum, unique))

    override def indexed[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        unique: Boolean
    ): T[Vector[A]] = fK(self.indexed(schema, minimum, maximum, unique))

    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object CollectionSchema:
  inline def apply[Self[_], Value[_]](using self: CollectionSchema[Self, Value]): CollectionSchema[Self, Value] =
    self
