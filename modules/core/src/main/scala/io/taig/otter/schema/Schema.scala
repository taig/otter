package io.taig.otter.schema

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Convert
import io.taig.otter.Merge
import io.taig.otter.Metadata

import scala.annotation.targetName
import scala.compiletime.*

trait Schema[Self[_]] extends Invariant[Self]:
  self =>

  final protected given Invariant[Self] = this

  extension [A](self: Self[A])
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      modifyMetadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] =
      metadata(key, value = value.some)

  extension [A](self: Self[A])
    // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
    // final def to[B](using convert: Convert[A, B]): F[B] = imap(convert.to)(convert.from)
    final inline def to[B]: Self[B] =
      val convert = summonInline[Convert[A, B]]
      self.imap(convert.to)(convert.from)

  extension [A, B](self: Self[(A, B)])
    final def merge(using merge: Merge[A, B]): Self[merge.Out] =
      self.imap(merge.apply)(merge.unapply)

  extension (self: Self[Unit])
    final def as[A](a: A): Self[A] = self.imap(_ => a)(_ => ())

    @targetName("asSingleton")
    final def as[A <: Singleton](a: A): Self[A] = self.imap(_ => a)(_ => ())

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema[T] = new Schema[T]:
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    extension [A](ta: T[A])
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object Schema:
  inline def apply[Self[_]](using schema: Schema[Self]): Schema[Self] = schema
