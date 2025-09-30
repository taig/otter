package io.taig.otter.operation

import io.taig.validation.Validation
import io.taig.validation.Constraint

trait CollectionSchemaInvariant[Self[_], -Value[_]] extends SchemaInvariant[Self]:
  self =>

  def linked[A](schema: => Value[A], validation: Validation[Constraint.Collection, List[A]]): Self[List[A]]

  def indexed[A](schema: => Value[A], validation: Validation[Constraint.Collection, Vector[A]]): Self[Vector[A]]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): CollectionSchemaInvariant[T, Value] = new CollectionSchemaInvariant[T, Value]:
    override def linked[A](
        schema: => Value[A],
        validation: Validation[Constraint.Collection, List[A]]
    ): T[List[A]] = fK(self.linked(schema, validation))

    override def indexed[A](
        schema: => Value[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): T[Vector[A]] = fK(self.indexed(schema, validation))

    override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object CollectionSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      self: CollectionSchemaInvariant[Self, Value]
  ): CollectionSchemaInvariant[Self, Value] = self
