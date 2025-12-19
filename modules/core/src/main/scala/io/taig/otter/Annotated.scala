package io.taig.otter

import cats.Invariant

trait Annotated[A]:
  self =>

  def get(self: A): Metadata

  def modify(self: A, metadata: Metadata => Metadata): A

  final def imap[B](f: A => B)(g: B => A): Annotated[B] = new Annotated[B]:
    def get(b: B): Metadata = self.get(g(b))

    def modify(b: B, metadata: Metadata => Metadata): B = f(self.modify(g(b), metadata))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated]:
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] =
      fa.imap(f)(g)
