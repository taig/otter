package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.validation.Validation
import cats.Invariant

trait CollectionBuilders extends Types:
  given collectionBuilderInvariant[A, B]: Invariant[CollectionBuilder[A, B, *]] with
    override def imap[C, D](fa: CollectionBuilder[A, B, C])(f: C => D)(g: D => C): CollectionBuilder[A, B, D] = ???

  def collection[A, B, C](f: Validation[A, Constraint.Collection, B, C])(g: C => A): CollectionBuilder[A, B, C] =
    new Base.CollectionBuilder:
      override def validation: Validation[A, Constraint.Collection, B, C] = f
      override def from(c: C): A = g(c)

  def collection[A, B](f: A => B)(g: B => A): CollectionBuilder[A, Nothing, B] = collection(Validation.lift(f))(g)

  object collection:
    def reader[A, B, C](f: Validation[A, Constraint.Collection, B, C]): CollectionBuilder.Reader[A, B, C] =
      new Base.CollectionBuilder.Reader:
        override def validation: Validation[A, Constraint.Collection, B, C] = f

    def reader[A, B](f: A => B): CollectionBuilder.Reader[A, Nothing, B] = reader(Validation.lift(f))

    def writer[A, B](f: B => A): CollectionBuilder.Writer[A, B] = new Base.CollectionBuilder.Writer:
      override def from(b: B): A = f(b)

  def vector[A]: CollectionBuilder[Vector[A], Nothing, Vector[A]] = collection(identity[Vector[A]])(identity)
  def seq[A]: CollectionBuilder[Vector[A], Nothing, Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)
  def list[A]: CollectionBuilder[Vector[A], Nothing, Seq[A]] = vector[A].imap(_.toList)(_.toVector)
