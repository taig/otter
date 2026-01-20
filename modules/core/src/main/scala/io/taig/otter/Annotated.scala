package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait Annotated[A]:
  self =>

  extension (self: A)
    def lens: (Metadata, Metadata => A)

    final def metadata: Metadata = lens._1

    final def modify(f: Metadata => Metadata): A = lens._2(f(lens._1))

    final def attr[A](key: Metadata.Key[A], keys: Metadata.Key[A]*): Option[A] =
      keys.foldLeft(metadata.get(key))((result, key) => result.orElse(metadata.get(key)))

  final def imap[B](f: A => B)(g: B => A): Annotated[B] = new Annotated[B]:
    extension (b: B) override def lens: (Metadata, Metadata => B) = self.lens(g(b)).map(_.map(f))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated]:
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] =
      fa.imap(f)(g)
