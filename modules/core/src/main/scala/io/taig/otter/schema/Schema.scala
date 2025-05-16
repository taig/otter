package io.taig.otter.schema

import io.taig.otter.Metadata
import scala.annotation.targetName
import cats.Invariant

trait Schema[Self[_]] extends Invariant[Self]:
  self =>

  def metadata[A](self: Self[A]): Metadata
  def modifyMetadata[A](self: Self[A])(f: Metadata => Metadata): Self[A]

  // def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema[T] = new Schema[T]:
  //   extension [A](ta: T[A])
  //     override def metadata: Metadata = self.metadata(gK(ta))
  //     override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
  //     override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  // extension [A](self: Self[A])
  //   def metadata: Metadata
  //   def modifyMetadata(f: Metadata => Metadata): Self[A]
  //   def imap[B](f: A => B)(g: B => A): Self[B]

  //   final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
  //   final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = modifyMetadata(_.put(key, value))
