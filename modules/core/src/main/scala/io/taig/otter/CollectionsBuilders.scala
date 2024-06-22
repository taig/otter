package io.taig.otter

import io.taig.otter as Base
import cats.syntax.all.*

trait CollectionBuilders extends Types
// given collectionBuilderValidationInvariant[A]: ValidationInvariant[CollectionBuilder[A, *]] =
//   new Base.ValidationInvariant[AsSchema, CollectionBuilder[A, *]]:
//     extension [B](self: CollectionBuilder[A, B])
//       override def ivalidate[V1, V2, C](validation: Validation[B, V1, V2, C])(f: C => B): CollectionBuilder[A, C] =
//         new Base.CollectionBuilder[AsSchema, A, C]:
//           override def validation: Validation[A, Nothing, Nothing, C] = ???
//           override def from(c: C): A = self.from(f(c))

// def collection[A, B](f: Validation[A, Nothing, Int, B])(g: B => A): CollectionBuilder[A, B] = new Base.CollectionBuilder:
//   override def validation: Validation[A, Nothing, Int, B] = f
//   override def from(b: B): A = g(b)

// def collection[A, B](f: A => B)(g: B => A): CollectionBuilder[A, B] = collection(Validation.lift(f))(g)

// object collection:
//   def reader[A, B](f: Validation[A, Nothing, Int, B]): CollectionBuilder.Reader[A, B] = new Base.CollectionBuilder.Reader:
//     override def validation: Validation[A, Nothing, Int, B] = f

//   def reader[A, B](f: A => B): CollectionBuilder.Reader[A, B] = reader(Validation.lift(f))

//   def writer[A, B](f: B => A): CollectionBuilder.Writer[A, B] = new Base.CollectionBuilder.Writer:
//     override def from(b: B): A = f(b)

// def vector[A]: CollectionBuilder[Vector[A], Vector[A]] = collection(Validation.ask)(identity)
// def seq[A]: CollectionBuilder[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)
// def list[A]: CollectionBuilder[Vector[A], Seq[A]] = vector[A].imap(_.toList)(_.toVector)
