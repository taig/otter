package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.*
import io.taig.otter as Self
import org.typelevel.ci.CIString

sealed abstract class Headers[A]:
  def names: Chain[CIString]

object Headers:
  private[otter] object Empty extends Headers[Unit]:
    override def names: Chain[CIString] = Chain.empty

  final private[otter] case class Optional[A](self: Headers[A]) extends Headers[Option[A]]:
    export self.names

  final private[otter] case class Root[A](header: Header[A]) extends Headers[A]:
    override def names: Chain[CIString] = Chain.one(header.name)

  final private[otter] case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def names: Chain[CIString] = left.names ++ right.names
