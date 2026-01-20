package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait Annotated[T]:
  self =>

  extension (self: T)
    def lens: (Metadata, Metadata => T)

    final def metadata: Metadata = lens._1

    final def modify(f: Metadata => Metadata): T = lens._2(f(lens._1))

    final def attr[A](key: Metadata.Key[A], keys: Metadata.Key[A]*): Option[A] =
      keys.foldLeft(metadata.get(key))((result, key) => result.orElse(metadata.get(key)))

    final def attr[A](key: Metadata.Key[A], value: A): T = modify(_.put(key, value))

  final def imap[B](f: T => B)(g: B => T): Annotated[B] = new Annotated[B]:
    extension (b: B) override def lens: (Metadata, Metadata => B) = self.lens(g(b)).map(_.map(f))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated]:
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] =
      fa.imap(f)(g)
