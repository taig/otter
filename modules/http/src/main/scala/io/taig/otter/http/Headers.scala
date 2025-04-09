package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.*
import io.taig.otter as Self

sealed abstract class Headers[A]:
  def toChain: Chain[Header[?]]

  final def imap[B](f: A => B)(g: B => A): Headers[B] = Headers.Modify(self = this, f, g)

  final def zip[B](headers: Headers[B]): Headers[(A, B)] = Headers.Zip(left = this, right = headers)

object Headers:
  private[otter] object Empty extends Headers[Unit]:
    override def toChain: Chain[Nothing] = Chain.empty

  final private[otter] case class Modify[A, B](self: Headers[A], f: A => B, g: B => A) extends Headers[B]:
    export self.toChain

  final private[otter] case class Optional[A](self: Headers[A]) extends Headers[Option[A]]:
    export self.toChain

  final private[otter] case class Root[A](header: Header[A]) extends Headers[A]:
    override def toChain: Chain[Header[A]] = Chain.one(header)

  final private[otter] case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def toChain: Chain[Header[?]] = left.toChain ++ right.toChain

  given invariant: Invariant.Product[Headers, Headers, Headers] with
    override def result: Invariant[Headers] = this
    override def fromElement[A](codec: Headers[A]): Headers[A] = codec

    extension [A](self: Headers[A])
      override def imap[B](f: A => B)(g: B => A): Headers[B] = self.imap(f)(g)
      override def zip[B](codec: Headers[B]): Headers[(A, B)] = self.zip(codec)
