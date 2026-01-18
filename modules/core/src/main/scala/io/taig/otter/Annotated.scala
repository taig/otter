package io.taig.otter

import cats.Invariant

trait Annotated[A]:
  self =>

  extension (self: A)
    final def attr[A](key: Metadata.Key[A], keys: Metadata.Key[A]*): Option[A] =
      keys.foldLeft(metadata.get(key))((result, key) => result.orElse(metadata.get(key)))

    def metadata: Metadata

    def modify(f: Metadata => Metadata): A

  final def imap[B](f: A => B)(g: B => A): Annotated[B] = new Annotated[B]:
    extension (b: B)
      override def metadata: Metadata = self.metadata(g(b))

      override def modify(h: Metadata => Metadata): B = f(self.modify(g(b))(h))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated]:
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] =
      fa.imap(f)(g)
