package io.taig.otter.operation

import io.taig.otter.Metadata

trait CollectionSchemaInvariant[Self[_], -Value[_]] extends SchemaInvariant[Self]:
  self =>

  def linked[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], unique: Boolean): Self[List[A]]

  def indexed[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], unique: Boolean): Self[Vector[A]]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): CollectionSchemaInvariant[T, Value] = new CollectionSchemaInvariant[T, Value]:
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

    override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object CollectionSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      self: CollectionSchemaInvariant[Self, Value]
  ): CollectionSchemaInvariant[Self, Value] = self
