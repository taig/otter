package io.taig.otter.operation

import io.taig.otter.Record
import io.taig.otter.Merge
import io.taig.otter.Annotation
import io.taig.otter.Field
import io.taig.otter.Reference

trait RecordSchemaInvariant[Self[_], -Field[_]] extends SchemaInvariant[Self]:
  self =>

  def empty: Self[Unit]

  def lift[A](field: => Field[A]): Self[A]

  extension [A](self: Self[A])
    def zip[B](schema: Self[B]): Self[(A, B)]

    final def merge[B](schema: Self[B])(using merge: Merge[A, B]): Self[merge.Out] = zip(schema).merged

    final def :*[B](field: => Field[B])(using merge: Merge[A, B]): Self[merge.Out] = self.merge(lift(field))

  extension [A](self: Field[A])
    final def *:[B](schema: => Self[B])(using merge: Merge[A, B]): Self[merge.Out] = lift(self).merge(schema)

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): RecordSchemaInvariant[T, Field] =
    new RecordSchemaInvariant[T, Field]:
      override def empty: T[Unit] = fK(self.empty)

      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))

      extension [A](ta: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object RecordSchemaInvariant:
  inline def apply[Self[_], Field[_]](using
      invariant: RecordSchemaInvariant[Self, Field]
  ): RecordSchemaInvariant[Self, Field] = invariant

  given [S[_]]: RecordSchemaInvariant[
    [a] =>> Annotation[Record[[b] =>> Annotation[Field[S, b]], a]],
    [a] =>> Annotation[Field[S, a]]
  ] with
    override def empty: Annotation[Record[[a] =>> Annotation[Field[S, a]], Unit]] = Annotation(Record.Empty)

    override def lift[A](field: => Annotation[Field[S, A]]): Annotation[Record[[a] =>> Annotation[Field[S, a]], A]] =
      Annotation(Record.Root(field = Reference.later(field)))

    extension [A](self: Annotation[Record[[a] =>> Annotation[Field[S, a]], A]])
      override def zip[B](
          schema: Annotation[Record[[a] =>> Annotation[Field[S, a]], B]]
      ): Annotation[Record[[a] =>> Annotation[Field[S, a]], (A, B)]] =
        Annotation(Record.Zip(left = self.self, right = schema.self))

      override def imap[B](f: A => B)(g: B => A): Annotation[Record[[a] =>> Annotation[Field[S, a]], B]] =
        self.map(_.imap(f)(g))
