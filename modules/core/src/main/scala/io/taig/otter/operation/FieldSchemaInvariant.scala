package io.taig.otter.operation

import io.taig.otter.Annotation
import io.taig.otter.Field
import io.taig.otter.Reference

trait FieldSchemaInvariant[Self[_], -Value[_]] extends SchemaInvariant[Self]:
  def apply[A](name: String, value: => Value[A]): Self[A]

  extension [A](self: Self[A]) def optional: Self[Option[A]]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): SchemaInvariant[G] = ???

object FieldSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      invariant: FieldSchemaInvariant[Self, Value]
  ): FieldSchemaInvariant[Self, Value] = invariant

  given schema[S[_]]: FieldSchemaInvariant[[a] =>> Annotation[Field[S, a]], S] with
    override def apply[A](name: String, value: => S[A]): Annotation[Field[S, A]] =
      Annotation(Field.Root(name, schema = Reference.later(value)))

    extension [A](self: Annotation[Field[S, A]])
      override def optional: Annotation[Field[S, Option[A]]] = Annotation(self.self.optional)

      override def imap[B](f: A => B)(g: B => A): Annotation[Field[S, B]] = self.map(_.imap(f)(g))
