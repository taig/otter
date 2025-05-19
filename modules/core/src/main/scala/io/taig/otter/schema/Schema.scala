package io.taig.otter.schema

import cats.Invariant
import io.taig.otter.Metadata

trait Schema[Self[_]] extends Invariant[Self]:
  self =>

  final protected given Invariant[Self] = this

  extension [A](self: Self[A])
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      modifyMetadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema[T] = new Schema[T]:
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object Schema:
  inline def apply[Self[_]](using schema: Schema[Self]): Schema[Self] = schema
