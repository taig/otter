package io.taig.otter.operation

import cats.syntax.all.*
import io.taig.otter.Annotation
import io.taig.otter.Primitive

trait BooleanSchemaInvariant[Self[_]] extends SchemaInvariant[Self]:
  self =>

  def boolean: Self[Boolean]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): BooleanSchemaInvariant[G] =
    new BooleanSchemaInvariant[G]:
      override def boolean: G[Boolean] = fK(self.boolean)
      extension [A](ga: G[A]) override def imap[B](f: A => B)(g: B => A): G[B] = fK(self.imap(gK(ga))(f)(g))

object BooleanSchemaInvariant:
  inline def apply[Self[_]](using invariant: BooleanSchemaInvariant[Self]): BooleanSchemaInvariant[Self] = invariant

  given schema: BooleanSchemaInvariant[[a] =>> Annotation[Primitive.Boolean[a]]] with
    override def boolean: Annotation[Primitive.Boolean[Boolean]] = Annotation(Primitive.Boolean.Root)
    extension [A](fa: Annotation[Primitive.Boolean[A]])
      override def imap[B](f: A => B)(g: B => A): Annotation[Primitive.Boolean[B]] = fa.map(_.imap(f)(g))
