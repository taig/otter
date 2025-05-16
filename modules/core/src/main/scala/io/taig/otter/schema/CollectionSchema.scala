package io.taig.otter.schema

import io.taig.otter.Metadata

trait CollectionSchema[Self[_], -Value[_]] extends Schema[Self]:
  self =>

  // final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
  //     gK: [A] => T[A] => Self[A]
  // ): CollectionSchema[T, Value] = new CollectionSchema[T, Value]:
  //   override def linked[A](
  //       schema: => Value[A],
  //       minimum: Option[Int],
  //       maximum: Option[Int],
  //       uniqueItems: Boolean
  //   ): T[List[A]] = fK(self.linked(schema, minimum, maximum, uniqueItems))

  //   override def indexed[A](
  //       schema: => Value[A],
  //       minimum: Option[Int],
  //       maximum: Option[Int],
  //       uniqueItems: Boolean
  //   ): T[Vector[A]] = fK(self.indexed(schema, minimum, maximum, uniqueItems))

  //   extension [A](ta: T[A])
  //     override def metadata: Metadata = self.metadata(gK(ta))
  //     override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
  //     override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  def linked[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], uniqueItems: Boolean): Self[List[A]]

  def indexed[A](
      schema: => Value[A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean
  ): Self[Vector[A]]

object CollectionSchema:
  inline def apply[Self[_], Value[_]](using self: CollectionSchema[Self, Value]): CollectionSchema[Self, Value] =
    self
