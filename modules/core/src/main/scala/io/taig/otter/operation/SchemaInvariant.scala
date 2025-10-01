package io.taig.otter.operation

import cats.Invariant
import io.taig.otter.Merge
import io.taig.otter.Annotation
import io.taig.otter.Schema

trait SchemaInvariant[Self[_]]:
  self =>

  def invariant: Invariant[Self] = new Invariant[Self]:
    override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = self.imap(fa)(f)(g)

  extension [A](self: Self[A]) def imap[B](f: A => B)(g: B => A): Self[B]

  extension [A, B](self: Self[(A, B)])
    final def merged(using merge: Merge[A, B]): Self[merge.Out] =
      self.imap(merge.apply)(merge.unapply)

  extension (self: Self[Unit]) final def as[A](a: A): Self[A] = self.imap(_ => a)(_ => ())

  def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): SchemaInvariant[G] =
    new SchemaInvariant[G]:
      extension [A](ga: G[A]) override def imap[B](f: A => B)(g: B => A): G[B] = fK(self.imap(gK(ga))(f)(g))

object SchemaInvariant:
  inline def apply[Self[_]](using invariant: SchemaInvariant[Self]): SchemaInvariant[Self] = invariant

  given schema: SchemaInvariant[[a] =>> Annotation[Schema[a]]] with
    extension [A](self: Annotation[Schema[A]])
      override def imap[B](f: A => B)(g: B => A): Annotation[Schema[B]] =
        self.map(_.imap(f)(g))
