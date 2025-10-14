package io.taig.otter

import cats.Invariant
import scala.deriving.Mirror
import scala.compiletime.*
import scala.Tuple.fromProductTyped
import shapeless3.deriving.K0

trait Annotated[A]:
  self =>

  def get(self: A): Metadata

  def update(self: A, metadata: Metadata => Metadata): A

  final def imap[B](f: A => B)(g: B => A): Annotated[B] = new Annotated[B]:
    def get(b: B): Metadata = self.get(g(b))

    def update(b: B, metadata: Metadata => Metadata): B =
      f(self.update(g(b), metadata))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated] with
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] =
      fa.imap(f)(g)
