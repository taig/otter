package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.*
import io.taig.otter as Self
import org.typelevel.ci.CIString

sealed abstract class Headers[A]:
  def names: Chain[CIString]

  final def imap[B](f: A => B)(g: B => A): Headers[B] = Headers.Modify(self = this, f, g)

  final def zip[B](headers: Headers[B]): Headers[(A, B)] = Headers.Zip(left = this, right = headers)

object Headers:
  private[otter] object Empty extends Headers[Unit]:
    override def names: Chain[CIString] = Chain.empty

  final private[otter] case class Modify[A, B](self: Headers[A], f: A => B, g: B => A) extends Headers[B]:
    export self.names

  final private[otter] case class Optional[A](self: Headers[A]) extends Headers[Option[A]]:
    export self.names

  final private[otter] case class Root[A](header: Header[A]) extends Headers[A]:
    override def names: Chain[CIString] = Chain.one(header.name)

  final private[otter] case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def names: Chain[CIString] = left.names ++ right.names

  given invariant: Invariant.Product[Headers, Headers, Headers] with
    override def result: Invariant[Headers] = this
    override def fromElement[A](codec: Headers[A]): Headers[A] = codec

    extension [A](self: Headers[A])
      override def imap[B](f: A => B)(g: B => A): Headers[B] = self.imap(f)(g)
      override def zip[B](codec: Headers[B]): Headers[(A, B)] = self.zip(codec)
